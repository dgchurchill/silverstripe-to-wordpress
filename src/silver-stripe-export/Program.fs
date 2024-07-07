open WordPressPCL.Models

task {
    let! wordpress = Wordpress.createClient Config.wordpressConfig

    let silverstripe = Silverstripe.Sql.GetDataContext().Silverstripe

    let exportPages =
        silverstripe.SiteTreeLive
        |> Seq.where (fun page -> page.ShowInMenus <> 0uy || page.ShowInSearch <> 0uy)  // pages that have ShowInMenus and ShowInSearch false are error pages or otherwise not visible

    for page in exportPages |> Seq.take 3 do
        printfn "Processing '%s'..." page.Title
        printfn "%s" page.Content
        let post = Post()
        post.Title <- Title page.Title
        post.Content <- Content page.Content
        //post.Type <- 
        let! result = wordpress.Posts.CreateAsync(post)
        printfn "created page %i" result.Id
        printfn ""
}
|> fun x -> x.Wait()
