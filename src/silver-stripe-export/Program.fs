open FSharp.Control
open FSharp.Json
open Microsoft.Extensions.Logging
open System.IO
open WordPressPCL.Models
open System.Threading
open System

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

type Media = {
    SilverstripeId : int
    WordPressId : int
    WordPressSourceUrl : string
}

task {
    let logger = loggerFactory.CreateLogger("main")
    
    let! wordpress = Wordpress.createClient Config.wordpressConfig

    let silverstripe = Silverstripe.Sql.GetDataContext().Silverstripe

    // todo: may need to join these to the posts?
    //dump (Some 10) (silverstripe.MeetingEventPageLive |> Seq.map (fun x -> x.ColumnValues))


    // for file in silverstripe.FileLive do
    //     printfn "%i; %s; %s; %s; %s; %s; %i; %i; %s" file.Id file.ClassName file.FileFilename file.FileHash file.FileVariant file.Name file.OwnerId file.ParentId file.Title

    // silverstripe.SiteTreeLive
    // |> Seq.groupBy (fun x -> x.ClassName)
    // |> Seq.map (fun (k, xs) -> k, Seq.length xs)
    // |> Seq.iter (printfn "%A")

    let files =
        silverstripe.FileLive
        |> Seq.map (fun x -> x.Id, x)
        |> Map.ofSeq

    let exportPages =
        silverstripe.SiteTreeLive
        |> Seq.where (fun page -> page.ShowInMenus <> 0uy || page.ShowInSearch <> 0uy)  // pages that have ShowInMenus and ShowInSearch false are error pages or otherwise not visible
      //  |> Seq.take 3  // todo: remove
        |> List.ofSeq

    let requiredFilesForPage (page : Silverstripe.Sql.dataContext.``silverstripe.SiteTree_LiveEntity``) : int seq =
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

    let dryRun = true

    let! uploadedMedia =
        requiredFiles
        |> Seq.map (fun file -> task {
            match uploadedMediaStateDict |> Map.tryFind file.Id with
            | Some media -> return media
            | None ->

            logger.LogInformation("Uploading {filename}...", file.Name)
            if file.FileVariant <> "" then
                failwithf "file %i '%s' '%s' has non-null variant" file.Id file.Name file.FileVariant

            let path = Path.Combine(Config.silverstripeAssetBasePath, (Silverstripe.getFilesystemPath file.FileFilename file.FileHash))

            if dryRun then
                return {
                    Media.SilverstripeId = file.Id
                    WordPressId = -file.Id
                    WordPressSourceUrl = sprintf "dryrun-placeholder-url-%i" file.Id
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

    File.WriteAllText(uploadedMediaStateFile, Json.serialize uploadedMedia)

    let wordpressMedia =
        uploadedMedia
        |> Seq.map (fun x -> x.SilverstripeId, x)
        |> Map.ofSeq

    for page in exportPages do
        logger.LogInformation("Processing '{title}'...", page.Title)

        let mutable content = page.Content

        let shortcodes = Silverstripe.findShortcodes page.Content
        for shortcode in shortcodes do
            logger.LogDebug("{shortcode}", sprintf "%A" shortcode)
    
            match shortcode.Kind with
            | Silverstripe.Image image ->
                let media = wordpressMedia[image.Id]
                content <- content.Replace(shortcode.Text, $"""<!-- wp:image {{"id":{media.WordPressId}}} --><figure class="wp-block-image"><img src="{media.WordPressSourceUrl}" class="wp-image-{media.WordPressId}" /></figure><!-- /wp:image -->""")
    
            | Silverstripe.FileLink id ->
                match wordpressMedia |> Map.tryFind id with
                | Some media ->
                    // Silverstripe file_link shortcodes are used to splice the file url into a href attribute, so just replace them with the url to the file.
                    content <- content.Replace(shortcode.Text, media.WordPressSourceUrl)
                | None ->
                    logger.LogWarning("File with id {id} not found", id)
            | x ->
                logger.LogWarning("Shortcode {shortcode} not supported", sprintf "%A" x)

        // if page.Title = "2023 Newsletters" then
        //     printfn "%s" page.Content
        //     printfn "----"
        //     printfn "%s" content

        let post = Post()
        post.Title <- Title (page.Title + " v8")
        post.Date <- page.LastEdited
        post.Content <- Content content
        post.Type <-
            match page.ClassName with
            | "ArticleHolder"
            | "HomePage"
            | "MeetEventTopPage"
            | "NewsletterHolder"
            | "Page" -> "page"
            
            | "MeetingEventPage" -> "post"
            
            | x ->
                logger.LogWarning("unhandled ClassName {className}", x)
                "post"
        post.Status <- Status.Publish  // todo: draft for drafts

        if not dryRun then
            let! result = wordpress.Posts.CreateAsync(post)
            logger.LogInformation("created page {id}", result.Id)
}
|> fun x -> x.Wait()

loggerFactory.Dispose()

// Wait for console logger to process its queue. On Dispose it will only wait
// for up to 1.5s to clear the queue, and we might have more to go...
// https://github.com/dotnet/runtime/blob/894f22d768e510fddb34259eca1107a5b26c9415/src/libraries/Microsoft.Extensions.Logging.Console/src/ConsoleLoggerProcessor.cs#L191
Thread.Sleep(TimeSpan.FromSeconds 1)
printfn "end."
