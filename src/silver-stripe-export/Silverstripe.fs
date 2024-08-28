(*
   
    silver-stripe-export Convert a Silverstripe site to Wordpress
    Copyright (C) 2024 David G. Churchill

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
    
*)

module Silverstripe

open FParsec
open FSharp.Data.Sql
open System
open System.Text.RegularExpressions
open System.IO

let [<Literal>] resolutionPath = __SOURCE_DIRECTORY__ + "/design-time/"

type Sql =
    SqlDataProvider<
        DatabaseVendor=Common.DatabaseProviderTypes.MYSQL,
        ResolutionPath=resolutionPath,
        ConnectionString=Config.connectionString>

let inline dump<'a when 'a:(member ColumnValues: seq<string * obj>)> (maybeLimit : int option) (table : 'a seq) =
    let rows : seq<seq<string * obj>> = table |> Seq.map (fun x -> x.ColumnValues)
    
    let limitedRows =
        match maybeLimit with
        | Some limit -> rows |> Seq.mapi (fun i x -> i, x) |> Seq.takeWhile (fst >> (>) limit) |> Seq.map snd
        | None -> rows

    for row in limitedRows do
        for key, value in row do
            if key = "Content" then
                printf "%s: %O; " key "<content>"
            else
                printf "%s: %O; " key value

        printfn ""

type MeetingEventPageData = {
    Date : DateTime
    Location : string
}

type Page = {
    Id : int
    Version : int
    Title : string
    ParentId : int
    ClassName : string
    Content : string
    LastEdited : DateTime
    ExtensionData : MeetingEventPageData option
}

/// Returns all visible pages as a sequence of tuples.
/// First is the published page (if published), second is the draft page.
/// If both published and draft are at the same version, then the
/// page hasn't had changes after being published.
/// If there is no published version then the page is just a draft.
let pages (silverstripe : Sql.dataContext.silverstripeSchema) =
    let liveMeetingEventData =
        silverstripe.MeetingEventPageLive
        |> Seq.map (fun x ->
            x.Id, {
                MeetingEventPageData.Date = x.Date
                Location = x.Location
        })
        |> Map.ofSeq

    let draftMeetingEventData =
        silverstripe.MeetingEventPage
        |> Seq.map (fun x ->
            x.Id, {
                MeetingEventPageData.Date = x.Date
                Location = x.Location
        })
        |> Map.ofSeq

    let livePages : Map<int, Page> =
        silverstripe.SiteTreeLive
        |> Seq.where (fun page -> page.ShowInMenus <> 0uy || page.ShowInSearch <> 0uy)  // pages that have ShowInMenus and ShowInSearch false are error pages or otherwise not visible
        |> Seq.map (fun x -> x.Id, {
            Id = x.Id
            Version = x.Version
            Title = x.Title
            ParentId = x.ParentId
            ClassName = x.ClassName
            Content = x.Content
            LastEdited = x.LastEdited
            ExtensionData = liveMeetingEventData |> Map.tryFind x.Id
        })
        |> Map.ofSeq

    let draftPages =
        silverstripe.SiteTree
        |> Seq.where (fun page -> page.ShowInMenus <> 0uy || page.ShowInSearch <> 0uy)  // pages that have ShowInMenus and ShowInSearch false are error pages or otherwise not visible
        |> Seq.map (fun x -> x.Id, {
            Id = x.Id
            Version = x.Version
            Title = x.Title
            ParentId = x.ParentId
            ClassName = x.ClassName
            Content = x.Content
            LastEdited = x.LastEdited
            ExtensionData = draftMeetingEventData |> Map.tryFind x.Id
        })
        |> Map.ofSeq

    let ids =
        Seq.concat [
            livePages |> Map.toSeq |> Seq.map fst
            draftPages |> Map.toSeq |> Seq.map fst
        ]
        |> Seq.distinct

    ids
    |> Seq.map (fun x -> livePages |> Map.tryFind x, draftPages |> Map.tryFind x)

// Behaviour based on getFileID at https://github.com/silverstripe/silverstripe-assets/blob/1.1.2/src/Flysystem/FlysystemAssetStore.php#L845
let getFilesystemPath (path : string) (hash : string) =
    let path = Regex.Replace(path, "__+", "_")
    $"{Path.GetDirectoryName path}/{hash.Substring(0, 10)}/{Path.GetFileName path}"

type ImageAttributes = {
    Id : int
    Source : string
    Width : int
    Height : int
    Classes : string list
    Title : string
}

type ShortcodeKind =
    | Image of attributes:ImageAttributes
    | FileLink of id:int
    | SiteTreeLink of id:int
    | Unknown

type Shortcode = {
    Text : string
    Kind : ShortcodeKind
}

let withRawText (p : Parser<'a, 'u>) : Parser<'a * string, 'u> =
    fun stream ->
        let startIndex = stream.Index
        let reply = p stream
        if reply.Status = Ok then
            let length = int (stream.Index - startIndex)
            stream.Seek startIndex
            let rawText = stream.Read length
            Reply ((reply.Result, rawText))
        else
            Reply (reply.Status, reply.Error)

let pshortcode =
    let ident =
        many1Satisfy (fun c -> isAsciiLetter c || c = '_')
    
    let shortcodeWithId command =
        skipString command >>. skipString ",id=" >>. pint32

    let quotedString = between (skipChar '"') (skipChar '"') (manySatisfy (isNoneOf "\""))

    let attribute =
        ident .>> pchar '=' .>>. quotedString

    let shortcodeWithAttributes command =
        skipString command >>. spaces1 >>. many (attribute .>> spaces)

    let unknownShortcode =
        ident >>. manySatisfy ((<>) ']') |>> (fun _ -> Unknown)

    let shortcodeBody =
        shortcodeWithId "file_link" |>> FileLink
        <|> (shortcodeWithId "sitetree_link" |>> SiteTreeLink)
        <|> (shortcodeWithAttributes "image" |>> fun attribs ->
             Image {
                 Id = attribs |> Seq.find (fst >> (=) "id") |> snd |> Int32.Parse
                 Source = attribs |> Seq.find (fst >> (=) "src") |> snd
                 Title = attribs |> Seq.find (fst >> (=) "title") |> snd
                 Width = attribs |> Seq.find (fst >> (=) "width") |> snd |> Int32.Parse
                 Height = attribs |> Seq.find (fst >> (=) "height") |> snd |> Int32.Parse
                 Classes = attribs |> Seq.find (fst >> (=) "class") |> snd |> (fun x -> x.Split(" ") |> List.ofArray)
             })
        <|> unknownShortcode

    withRawText (between (skipString "[") (skipString "]") shortcodeBody)
    |>> (fun (kind, text) -> { Text = text; Kind = kind })

let manyShortcodes =
    let maybeShortcode =
        attempt pshortcode |>> Some
        <|> (skipAnyChar |>> (fun _ -> None))

    many maybeShortcode |>> List.choose id

let findShortcodes (content : string) : Shortcode list =
    match run manyShortcodes content with
    | Success (result, (), _) -> result
    | Failure (errorAsString, error, ()) -> failwithf "shortcode parser failed with %s. %A" errorAsString error
