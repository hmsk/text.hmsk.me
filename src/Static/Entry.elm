module Static.Entry exposing (main)

import Css exposing (..)
import Html exposing (Html)
import Html.Styled exposing (a, article, div, fromUnstyled, h2, h3, p, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra exposing (datetime)
import Markdown
import Regex
import Siteelm.Html as Html
import Siteelm.Html.Attributes exposing (content, property)
import Siteelm.Page exposing (Page, page)
import Static.View as View
import Time exposing (..)


main : Page Preamble
main =
    page
        { decoder = preambleDecoder
        , head = viewHead
        , body = viewBody
        }


type alias Preamble =
    { title : String
    , date : Posix
    , url : String
    , originalUrl : Maybe String
    }


preambleDecoder : Decoder Preamble
preambleDecoder =
    D.map4 Preamble
        (D.field "title" D.string)
        (D.field "date" datetime)
        (D.field "url" D.string)
        (D.field "original_url" D.string |> D.maybe)


viewHead : Preamble -> String -> List (Html Never)
viewHead preamble _ =
    let
        title =
            preamble.title ++ " | text.hmsk.me"
        url =
            "https://text.hmsk.me/entries" ++ preamble.url
    in
    [ Html.title [] title
    , Html.script "https://cdn.iframe.ly/embed.js" ""
    , Html.meta [ Siteelm.Html.Attributes.property "og:title", Siteelm.Html.Attributes.content title ]
    , Html.meta [ Siteelm.Html.Attributes.property "og:url", Siteelm.Html.Attributes.content url ]
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
            , h3 [] [ text <| dateFormat preamble.date ]
            , linkForOriginal preamble.originalUrl
            , div
                [ css [ lineHeight (num 1.8) ]
                ]
                [ fromUnstyled <| Markdown.toHtmlWith markedOptions [] processedBody
                ]
            ]
        , View.footer
        ]


dateFormat : Posix -> String
dateFormat posix =
    let
        y =
            String.fromInt <| toYear utc posix

        m =
            toHumanMonth <| toMonth utc posix

        d =
            String.fromInt <| toDay utc posix
    in
    String.concat [ m, " ", d, ", ", y ]


linkForOriginal : Maybe String -> Html.Styled.Html Never
linkForOriginal maybeOriginal =
    case maybeOriginal of
        Just link ->
            p []
                [ text "公開時のURL:"
                , a [ href link ] [ text link ]
                ]

        _ ->
            text ""


toHumanMonth : Month -> String
toHumanMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "Feburary"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


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
