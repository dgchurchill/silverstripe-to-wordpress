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

// Behaviour based on getFileID at https://github.com/silverstripe/silverstripe-assets/blob/1.1.2/src/Flysystem/FlysystemAssetStore.php#L845
let getFilesystemPath (path : string) (hash : string) =
    let path = Regex.Replace(path, "__+", "_")
    $"{Path.GetDirectoryName path}/{hash.Substring(0, 10)}/${Path.GetFileName path}"

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
