module Static.Entry exposing (main)

import Css exposing (..)
import Html exposing (Html)
import Html.Styled exposing (article, div, fromUnstyled, h2, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Json.Decode as D exposing (Decoder)
import Markdown
import Regex
import Siteelm.Html as Html
import Siteelm.Page exposing (Page, page)
import Static.View as View


main : Page Preamble
main =
    page
        { decoder = preambleDecoder
        , head = viewHead
        , body = viewBody
        }


type alias Preamble =
    { title : String }


preambleDecoder : Decoder Preamble
preambleDecoder =
    D.map Preamble
        (D.field "title" D.string)


viewHead : Preamble -> String -> List (Html Never)
viewHead preamble _ =
    [ Html.title [] (preamble.title ++ " | text.hmsk.me")
    , Html.script "https://cdn.iframe.ly/embed.js" ""
    ]


viewBody : Preamble -> String -> List (Html Never)
viewBody preamble body =
    let
        processedBody =
            body |> replacer Amazon |> replacer Instagram |> replacer EmbedCard
    in
    List.map
        toUnstyled
        [ View.header
        , article [ css [ maxWidth (px 1280), margin auto ] ]
            [ h2 [] [ text preamble.title ]
            , div []
                [ fromUnstyled <| Markdown.toHtmlWith markedOptions [] processedBody
                ]
            ]
        , View.footer
        ]


type CustomTagType
    = Amazon
    | Instagram
    | EmbedCard


replacer : CustomTagType -> String -> String
replacer tag original =
    let
        regexAndTag =
            case tag of
                Amazon ->
                    ( "\\[asin:(.+):detail\\]", amazon )

                Instagram ->
                    ( "\\[instagram:(.+)\\]", instagram )

                EmbedCard ->
                    ( "\\[embed:(.+)\\]", embedCard )
    in
    case Regex.fromString <| Tuple.first regexAndTag of
        Nothing ->
            original

        Just regex ->
            Regex.replace
                regex
                (.submatches >> Tuple.second regexAndTag)
                original


amazon : List (Maybe String) -> String
amazon list =
    case List.head list of
        Just a ->
            case a of
                Just b ->
                    "<div data-elm-module=\"Dynamic.Amazon\" data-flags=\"{ asin: '" ++ b ++ "'}\"></div>"

                _ ->
                    "Nothing"

        _ ->
            "Nothing"


instagram : List (Maybe String) -> String
instagram list =
    case List.head list of
        Just a ->
            case a of
                Just b ->
                    "<div data-elm-module=\"Dynamic.Instagram\" data-flags=\"{ id: '" ++ b ++ "'}\"></div>"

                _ ->
                    "Nothing"

        _ ->
            "Nothing"


embedCard : List (Maybe String) -> String
embedCard list =
    case List.head list of
        Just a ->
            case a of
                Just b ->
                    "<div data-elm-module=\"Dynamic.EmbedCard\" data-flags=\"{ url: '" ++ b ++ "', embed: '' }\"></div>"

                _ ->
                    "Nothing"

        _ ->
            "Nothing"


markedOptions : Markdown.Options
markedOptions =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Nothing
    , sanitize = False
    , smartypants = False
    }
