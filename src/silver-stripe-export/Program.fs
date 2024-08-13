open FSharp.Control
open FSharp.Json
open Microsoft.Extensions.Logging
open System.IO
open WordPressPCL.Models
open System.Threading
open System

let dryRun = true

let loggerFactory =
    LoggerFactory.Create(fun builder ->
        builder
            .AddSimpleConsole(fun options ->
                options.IncludeScopes <- true
                options.SingleLine <- true
                options.TimestampFormat <- "HH:mm:ss "
            )
            .SetMinimumLevel(LogLevel.Information)
            |> ignore)

type Media = {
    SilverstripeId : int
    WordPressId : int
    WordPressSourceUrl : string
}

type PublishedOrDraft =
    | Published
    | Draft

type PageType =
    | Page
    | Post

type PageState =
    | Created
    | Completed

type Page = {
    SilverstripeId : int
    WordPressId : int
    Title : string
    PageState : PageState
    PageType : PageType
    WordPressUrl : string
}

task {
    let logger = loggerFactory.CreateLogger("main")
    
    let! wordpress = Wordpress.createClient Config.wordpressConfig

    let! wordpressCategories = wordpress.Categories.GetAllAsync()
    let eventCategoryId = wordpressCategories |> Seq.find (fun x -> x.Name = "Event") |> fun x -> x.Id
    let meetingCategoryId = wordpressCategories |> Seq.find (fun x -> x.Name = "Meeting") |> fun x -> x.Id

    let silverstripe = Silverstripe.Sql.GetDataContext().Silverstripe

    let files =
        silverstripe.FileLive
        |> Seq.map (fun x -> x.Id, x)
        |> Map.ofSeq

    // load pages:
    // 1. from SiteTreeLive, get page and version, get corresponding value from MeetingEventPageLive
    // 2. from SiteTree, get page and version, get corresponding value from MeetingEventPage
    // 3. merge together based on page ids: if in just draft, then draft, or if draft version > live version then draft (but warn that there's also a published version...)

    let exportPages =
        seq {
            for published, draft in Silverstripe.pages silverstripe do
                match published, draft with
                | Some p, Some d ->
                    if d.Version > p.Version then
                        logger.LogWarning("Draft of page id {id} \"{title}\" is version {draftVersion} which is later than published version {publishedVersion}. Draft changes will be lost.", p.Id, p.Title, d.Version, p.Version)
                    yield p, Published
            
                | Some p, None ->
                    yield p, Published

                | None, Some d ->
                    yield d, Draft
                
                | None, None ->
                    failwith "Export page has no draft or published version"
        }
        |> List.ofSeq

    let requiredFilesForPage (page : Silverstripe.Page) : int seq =
        Silverstripe.findShortcodes page.Content
        |> Seq.choose (fun shortcode ->
            let maybeId =
                match shortcode.Kind with
                | Silverstripe.Image image -> Some image.Id
                | Silverstripe.FileLink id -> Some id
                | _ -> None

            match maybeId with
            | None -> None
            | Some id ->
                match files |> Map.tryFind id with
                | Some _ -> Some id
                | None ->
                    logger.LogWarning("Missing required file id {id} for page {title} in shortcode {shortcode}", id, page.Title, shortcode.Text)
                    None)

    let requiredFiles =
        exportPages
        |> Seq.collect (fst >> requiredFilesForPage)
        |> Set.ofSeq
        |> Seq.map (fun i -> files[i])
        |> List.ofSeq

    let uploadedMediaStateFile = "uploaded-media.json"
    
    let uploadedMediaState : Media list =
        if File.Exists uploadedMediaStateFile then
            File.ReadAllText uploadedMediaStateFile
            |> Json.deserialize
        else
            []

    let uploadedMediaStateDict =
        uploadedMediaState
        |> Seq.map (fun x -> x.SilverstripeId, x)
        |> Map.ofSeq

    let! uploadedMedia =
        requiredFiles
        |> Seq.map (fun file -> task {
            match uploadedMediaStateDict |> Map.tryFind file.Id with
            | Some media -> return media
            | None ->

            logger.LogInformation("Uploading {filename}...", file.Name)
            if file.FileVariant <> "" then
                failwithf "file %i '%s' '%s' has non-null variant" file.Id file.Name file.FileVariant

            let path = Path.Join(Config.silverstripeAssetBasePath, (Silverstripe.getFilesystemPath file.FileFilename file.FileHash))

            if dryRun then
                if not <| File.Exists path then
                    logger.LogWarning("Media file {id} at {path} does not exist", file.Id, path)
            
                return {
                    Media.SilverstripeId = file.Id
                    WordPressId = -file.Id
                    WordPressSourceUrl = sprintf "dryrun-placeholder-url-media-%i" file.Id
                }
            else
                let! mediaItem = wordpress.Media.CreateAsync(path, file.FileFilename)

                logger.LogDebug("wordpress media id = {id}", mediaItem.Id)

                return {
                    Media.SilverstripeId = file.Id
                    WordPressId = mediaItem.Id
                    WordPressSourceUrl = mediaItem.SourceUrl
                }
        })
        |> System.Threading.Tasks.Task.WhenAll

    if not dryRun then
        File.WriteAllText(uploadedMediaStateFile, Json.serialize uploadedMedia)

    let wordpressMedia =
        uploadedMedia
        |> Seq.map (fun x -> x.SilverstripeId, x)
        |> Map.ofSeq

    let uploadedPagesStateFile = "uploaded-pages.json"

    let uploadedPageState : Page list =
        if File.Exists uploadedPagesStateFile then
            File.ReadAllText uploadedPagesStateFile
            |> Json.deserialize
        else
            []

    let uploadedPageStateDict =
        uploadedPageState
        |> Seq.map (fun x -> x.SilverstripeId, x)
        |> Map.ofSeq


    // First, create all the pages with empty content. This ensures that we know what ids to use
    // for parent page references, site tree links, etc
    let pageType className : PageType =
        match className with
        | "ArticleHolder"
        | "HomePage"
        | "MeetEventTopPage"
        | "NewsletterHolder"
        | "Page" -> Page

        | "MeetingEventPage" -> Post

        | x ->
            logger.LogWarning("unhandled ClassName {className}", x)
            Post

    let! uploadedPages =
        exportPages
        |> Seq.map (fun (page, publishedOrDraft) -> task {
            match uploadedPageStateDict |> Map.tryFind page.Id with
            | Some page -> return page
            | None ->

            logger.LogInformation("Creating '{title}'...", page.Title)

            if dryRun then
                return {
                    Page.SilverstripeId = page.Id
                    WordPressId = -page.Id
                    Title = page.Title
                    PageState = Created
                    PageType = pageType page.ClassName
                    WordPressUrl = sprintf "dryrun-placeholder-url-page-%i" page.Id
                }
            else
            
            match pageType page.ClassName with
            | Page ->
                let post = WordPressPCL.Models.Page()
                post.Title <- Title page.Title
                post.Date <- page.LastEdited
                post.Status <- match publishedOrDraft with Published -> Status.Publish | Draft -> Status.Draft

                let! result = wordpress.Pages.CreateAsync(post)
                logger.LogInformation("created page {id}", result.Id)

                return {
                    Page.SilverstripeId = page.Id
                    WordPressId = result.Id
                    Title = page.Title
                    PageState = Created
                    PageType = Page
                    WordPressUrl = result.Link
                }

            | Post ->
                let post = WordPressPCL.Models.Post()
                post.Title <- Title page.Title
                post.Date <- page.LastEdited
                post.Status <- match publishedOrDraft with Published -> Status.Publish | Draft -> Status.Draft

                let! result = wordpress.Posts.CreateAsync(post)
                logger.LogInformation("created page {id}", result.Id)

                return {
                    Page.SilverstripeId = page.Id
                    WordPressId = result.Id
                    Title = page.Title
                    PageState = Created
                    PageType = Post
                    WordPressUrl = result.Link
                }
        })
        |> System.Threading.Tasks.Task.WhenAll

    if not dryRun then
        File.WriteAllText(uploadedPagesStateFile, Json.serialize uploadedPages)

    let mutable uploadedPageStateDict =
        uploadedPages
        |> Seq.map (fun x -> x.SilverstripeId, x)
        |> Map.ofSeq

    let eventsPage = uploadedPages |> Seq.find (fun x -> x.Title = "Events")
    let meetingsPage = uploadedPages |> Seq.find (fun x -> x.Title = "Meetings")

    // Now, set the content for each page
    for (page, publishedOrDraft) in exportPages do
        let uploadedPage = uploadedPageStateDict |> Map.find page.Id
        logger.LogInformation("Processing '{title}'...", page.Title)

        let mutable content = page.Content

        let shortcodes = Silverstripe.findShortcodes page.Content
        for shortcode in shortcodes do
            logger.LogDebug("{shortcode}", sprintf "%A" shortcode)

            match shortcode.Kind with
            | Silverstripe.Image image ->
                match wordpressMedia |> Map.tryFind image.Id with
                | Some media ->
                    content <- content.Replace(shortcode.Text, $"""<!-- wp:image {{"id":{media.WordPressId}}} --><figure class="wp-block-image"><img src="{media.WordPressSourceUrl}" class="wp-image-{media.WordPressId}" /></figure><!-- /wp:image -->""")
                | None ->
                    logger.LogWarning("Image with id {id} not found", image.Id)

            | Silverstripe.FileLink id ->
                match wordpressMedia |> Map.tryFind id with
                | Some media ->
                    // Silverstripe file_link shortcodes are used to splice the file url into a href attribute, so just replace them with the url to the file.
                    content <- content.Replace(shortcode.Text, media.WordPressSourceUrl)
                | None ->
                    logger.LogWarning("File with id {id} not found", id)

            | Silverstripe.SiteTreeLink id ->
                match uploadedPageStateDict |> Map.tryFind id with
                | Some page ->
                    content <- content.Replace(shortcode.Text, page.WordPressUrl)
                | None ->
                    logger.LogWarning("Page with id {id} not found", id)
            | x ->
                logger.LogWarning("Shortcode {shortcode} not supported", sprintf "%A" x)

        match page.ExtensionData with
        | Some extensionData ->
            content <- sprintf "<p><b>Date:</b> %s</p><p><b>Location:</b> %s</p>" (extensionData.Date.ToString "dddd, d MMMM yyyy") extensionData.Location + content
        | _ -> ()

        if not dryRun then
            match pageType page.ClassName with
            | Page ->
                let post = WordPressPCL.Models.Page()
                post.Id <- uploadedPage.WordPressId
                post.Title <- Title page.Title
                post.Date <- page.LastEdited
                post.Content <- Content content
                post.Status <- match publishedOrDraft with Published -> Status.Publish | Draft -> Status.Draft

                if page.ParentId <> 0 then
                    post.Parent <- uploadedPageStateDict[page.ParentId].WordPressId

                let! result = wordpress.Pages.UpdateAsync(post)
                logger.LogInformation("updated page {id}", result.Id)

            | Post ->
                let post = WordPressPCL.Models.Post()
                post.Id <- uploadedPage.WordPressId
                post.Title <- Title page.Title
                post.Date <- page.LastEdited
                post.Content <- Content content
                post.Status <- match publishedOrDraft with Published -> Status.Publish | Draft -> Status.Draft

                post.Categories <-
                    match page.ParentId with
                    | x when x = eventsPage.SilverstripeId -> ResizeArray [ eventCategoryId ]
                    | x when x = meetingsPage.SilverstripeId -> ResizeArray [ meetingCategoryId ]
                    | _ -> ResizeArray()

                let! result = wordpress.Posts.UpdateAsync(post)
                logger.LogInformation("updated page {id}", result.Id)

            uploadedPageStateDict <-
                uploadedPageStateDict
                |> Map.add page.Id { uploadedPage with PageState = Completed }

            File.WriteAllText(uploadedPagesStateFile, Json.serialize uploadedPages)
}
|> fun x -> x.Wait()

loggerFactory.Dispose()

// Wait for console logger to process its queue. On Dispose it will only wait
// for up to 1.5s to clear the queue, and we might have more to go...
// https://github.com/dotnet/runtime/blob/894f22d768e510fddb34259eca1107a5b26c9415/src/libraries/Microsoft.Extensions.Logging.Console/src/ConsoleLoggerProcessor.cs#L191
Thread.Sleep(TimeSpan.FromSeconds 5)
printfn "end."
