module Silverstripe

open FParsec
open FSharp.Data.Sql
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

type ShortcodeKind =
    | Image
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
    let shortcodeWithId command =
        pstring command >>. skipString ",id=" >>. pint32

    let unknownShortcode =
        many1Satisfy (fun c -> isAsciiLetter c || c = '_') >>. manySatisfy ((<>) ']') |>> (fun _ -> Unknown)

    let shortcodeBody =
        shortcodeWithId "file_link" |>> FileLink
        <|> (shortcodeWithId "sitetree_link" |>> SiteTreeLink)
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
