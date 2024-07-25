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

let dump (maybeLimit : int option) (rows : seq<seq<string * obj>>) =
    let limitedRows =
        match maybeLimit with
        | Some limit -> rows |> Seq.mapi (fun i x -> i, x) |> Seq.takeWhile (fst >> (>) limit) |> Seq.map snd
        | None -> rows

    for row in limitedRows do
        for key, value in row do
            printf "%s: %O; " key value
        printfn ""

type SilverstripePage = {
    Id : int
    Title : string
    ParentId : int
    ClassName : string
    Content : string
    IsDraft : bool
    LastEdited : DateTime
}

type Media = {
    SilverstripeId : int
    WordPressId : int
    WordPressSourceUrl : string
}

type PageType =
    | Page
    | Post

type Page = {
    SilverstripeId : int
    WordPressId : int
    PageType : PageType
    WordPressUrl : string
}

task {
    let logger = loggerFactory.CreateLogger("main")
    
    let! wordpress = Wordpress.createClient Config.wordpressConfig

    let silverstripe = Silverstripe.Sql.GetDataContext().Silverstripe

    // todo: may need to join these to the posts?
    //dump (Some 10) (silverstripe.MeetingEventPageLive |> Seq.map (fun x -> x.ColumnValues))


    // for file in silverstripe.FileLive do
    //     printfn "%i; %s; %s; %s; %s; %s; %i; %i; %s" file.Id file.ClassName file.FileFilename file.FileHash file.FileVariant file.Name file.OwnerId file.ParentId file.Title

    let files =
        silverstripe.FileLive
        |> Seq.map (fun x -> x.Id, x)
        |> Map.ofSeq

    // todo: verify that each page id only appears in either livePages OR draftPages

    let livePages : SilverstripePage seq =
        silverstripe.SiteTreeLive
        |> Seq.where (fun page -> page.ShowInMenus <> 0uy || page.ShowInSearch <> 0uy)  // pages that have ShowInMenus and ShowInSearch false are error pages or otherwise not visible
        |> Seq.map (fun x -> {
            Id = x.Id
            Title = x.Title
            ParentId = x.ParentId
            ClassName = x.ClassName
            Content = x.Content
            IsDraft = false
            LastEdited = x.LastEdited
        })

    let draftPages =
        silverstripe.SiteTree
        |> Seq.where (fun page -> page.ShowInMenus <> 0uy || page.ShowInSearch <> 0uy)  // pages that have ShowInMenus and ShowInSearch false are error pages or otherwise not visible
        |> Seq.map (fun x -> {
            Id = x.Id
            Title = x.Title
            ParentId = x.ParentId
            ClassName = x.ClassName
            Content = x.Content
            IsDraft = true
            LastEdited = x.LastEdited
        })

    let exportPages =
        Seq.concat [| livePages; draftPages |]
        |> List.ofSeq

    let requiredFilesForPage (page : SilverstripePage) : int seq =
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
        |> Seq.collect requiredFilesForPage
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

        | "MeetingEventPage" -> Page // Was Post, but seems like these have a hierarchy too...?

        | x ->
            logger.LogWarning("unhandled ClassName {className}", x)
            Post

    let! uploadedPages =
        exportPages
        |> Seq.map (fun page -> task {
            match uploadedPageStateDict |> Map.tryFind page.Id with
            | Some page -> return page
            | None ->

            logger.LogInformation("Creating '{title}'...", page.Title)

            if dryRun then
                return {
                    Page.SilverstripeId = page.Id
                    WordPressId = -page.Id
                    PageType = pageType page.ClassName
                    WordPressUrl = sprintf "dryrun-placeholder-url-page-%i" page.Id
                }
            else
            
            match pageType page.ClassName with
            | Page ->
                let post = WordPressPCL.Models.Page()
                post.Title <- Title (page.Title + " v8")
                post.Date <- page.LastEdited
                post.Status <- if page.IsDraft then Status.Draft else Status.Publish

                let! result = wordpress.Pages.CreateAsync(post)
                logger.LogInformation("created page {id}", result.Id)

                return {
                    Page.SilverstripeId = page.Id
                    WordPressId = result.Id
                    PageType = Page
                    WordPressUrl = result.Link
                }

            | Post ->
                let post = WordPressPCL.Models.Post()
                post.Title <- Title (page.Title + " v8")
                post.Date <- page.LastEdited
                post.Status <- if page.IsDraft then Status.Draft else Status.Publish

                let! result = wordpress.Posts.CreateAsync(post)
                logger.LogInformation("created page {id}", result.Id)

                return {
                    Page.SilverstripeId = page.Id
                    WordPressId = result.Id
                    PageType = Post
                    WordPressUrl = result.Link
                }
        })
        |> System.Threading.Tasks.Task.WhenAll

    if not dryRun then
        File.WriteAllText(uploadedPagesStateFile, Json.serialize uploadedPages)

    let uploadedPageStateDict =
        uploadedPages
        |> Seq.map (fun x -> x.SilverstripeId, x)
        |> Map.ofSeq

    // Now, set the content for each page
    for page in exportPages do
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

        // if page.Title = "Latest News" then
        //     printfn "%s" page.Content
        //     printfn "----"
        //     printfn "%s" content

        if not dryRun then
            match pageType page.ClassName with
            | Page ->
                let post = WordPressPCL.Models.Page()
                post.Id <- uploadedPage.WordPressId
                post.Title <- Title (page.Title + " v8")
                post.Date <- page.LastEdited
                post.Content <- Content content
                post.Status <- if page.IsDraft then Status.Draft else Status.Publish

                if page.ParentId <> 0 then
                    post.Parent <- uploadedPageStateDict[page.ParentId].WordPressId

                let! result = wordpress.Pages.UpdateAsync(post)
                logger.LogInformation("updated page {id}", result.Id)

            | Post ->
                let post = WordPressPCL.Models.Post()
                post.Id <- uploadedPage.WordPressId
                post.Title <- Title (page.Title + " v8")
                post.Date <- page.LastEdited
                post.Content <- Content content
                post.Status <- if page.IsDraft then Status.Draft else Status.Publish

                let! result = wordpress.Posts.UpdateAsync(post)
                logger.LogInformation("updated page {id}", result.Id)
}
|> fun x -> x.Wait()

loggerFactory.Dispose()

// Wait for console logger to process its queue. On Dispose it will only wait
// for up to 1.5s to clear the queue, and we might have more to go...
// https://github.com/dotnet/runtime/blob/894f22d768e510fddb34259eca1107a5b26c9415/src/libraries/Microsoft.Extensions.Logging.Console/src/ConsoleLoggerProcessor.cs#L191
Thread.Sleep(TimeSpan.FromSeconds 5)
printfn "end."
