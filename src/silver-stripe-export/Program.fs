open FSharp.Control
open FSharp.Json
open Microsoft.Extensions.Logging
open System.IO
open WordPressPCL.Models


let loggerFactory =
    LoggerFactory.Create(fun builder ->
        builder.AddSimpleConsole(fun options ->
            options.IncludeScopes <- true
            options.SingleLine <- true
            options.TimestampFormat <- "HH:mm:ss "
        ) |> ignore)

let logger = loggerFactory.CreateLogger("main")


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
                    WordPressId = -1
                    WordPressSourceUrl = null
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
            printfn "%A" shortcode
    
            match shortcode.Kind with
            | Silverstripe.Image image ->
                let file = files[image.Id]
                printfn "%i; %s; %s; %s; %s; %s; %i; %i; %s" file.Id file.ClassName file.FileFilename file.FileHash file.FileVariant file.Name file.OwnerId file.ParentId file.Title
                let media = wordpressMedia[image.Id]
                content <- content.Replace(shortcode.Text, $"""<!-- wp:image {{"id":{media.WordPressId}}} --><figure class="wp-block-image"><img src="{media.WordPressSourceUrl}" class="wp-image-{media.WordPressId}" /></figure><!-- /wp:image -->""")
    
            | Silverstripe.FileLink id ->
                match files |> Map.tryFind id with
                | Some file ->
                    printfn "%i; %s; %s; %s; %s; %s; %i; %i; %s" file.Id file.ClassName file.FileFilename file.FileHash file.FileVariant file.Name file.OwnerId file.ParentId file.Title
                | None ->
                    logger.LogWarning("File with id {id} not found", id)
            | _ -> ()

//        printfn "%s" page.Content

        let post = Post()
        post.Title <- Title (page.Title + " v8")
        post.Date <- page.LastEdited
        post.Content <- Content content
        post.Type <- // todo: maybe post for meetings?
            match page.ClassName with
            | "Page" -> "page"
            | x ->
                logger.LogWarning("unhandled ClassName {className}", x)
                "post"
        post.Status <- Status.Publish  // todo: draft for drafts

        if not dryRun then
            let! result = wordpress.Posts.CreateAsync(post)
            printfn "created page %i" result.Id
            printfn ""
}
|> fun x -> x.Wait()
