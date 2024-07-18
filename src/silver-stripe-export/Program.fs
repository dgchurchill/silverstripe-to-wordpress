open FSharp.Control
open System.IO
open WordPressPCL.Models
open FSharp.Json

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
        |> dict

    let exportPages =
        silverstripe.SiteTreeLive
        |> Seq.where (fun page -> page.ShowInMenus <> 0uy || page.ShowInSearch <> 0uy)  // pages that have ShowInMenus and ShowInSearch false are error pages or otherwise not visible
        |> Seq.take 3  // todo: remove
        |> List.ofSeq

    let requiredFiles =
        exportPages
        |> Seq.collect (fun page -> Silverstripe.findShortcodes page.Content)
        |> Seq.choose (fun shortcode ->
            match shortcode.Kind with
            | Silverstripe.Image image -> Some image.Id
            | Silverstripe.FileLink id -> Some id
            | _ -> None)
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

            printfn "Uploading %s..." file.Name
            if file.FileVariant <> "" then
                failwithf "file %i '%s' '%s' has non-null variant" file.Id file.Name file.FileVariant

            let path = Path.Combine(Config.silverstripeAssetBasePath, (Silverstripe.getFilesystemPath file.FileFilename file.FileHash))
            printfn "%s" path
            let! mediaItem = wordpress.Media.CreateAsync(path, file.FileFilename)

            printfn "wordpress media id = %i" mediaItem.Id

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

    for page in exportPages |> Seq.take 3 do
        printfn "Processing '%s'..." page.Title

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
                let file = files[id]
                printfn "%i; %s; %s; %s; %s; %s; %i; %i; %s" file.Id file.ClassName file.FileFilename file.FileHash file.FileVariant file.Name file.OwnerId file.ParentId file.Title
            | _ -> ()

//        printfn "%s" page.Content

        let post = Post()
        post.Title <- Title (page.Title + " v8")
        post.Date <- page.LastEdited
        post.Content <- Content content
        post.Type <- "page"   // todo: maybe post for meetings?
        post.Status <- Status.Publish  // todo: draft for drafts
        let! result = wordpress.Posts.CreateAsync(post)
        printfn "created page %i" result.Id
        printfn ""
}
|> fun x -> x.Wait()
