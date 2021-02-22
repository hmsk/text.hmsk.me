module Static.Entry exposing (main)

import Css
    exposing
        ( auto
        , backgroundColor
        , block
        , borderLeft3
        , color
        , display
        , hex
        , lineHeight
        , margin
        , margin2
        , margin3
        , maxWidth
        , num
        , overflow
        , padding
        , padding2
        , pct
        , px
        , rem
        , solid
        , width
        )
import Css.Global exposing (children, descendants, global, mediaQuery, typeSelector)
import Html exposing (Html)
import Html.Styled exposing (a, article, div, fromUnstyled, h2, h3, p, text, toUnstyled)
import Html.Styled.Attributes exposing (class, href)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra exposing (datetime)
import Markdown
import Regex
import Siteelm.Date exposing (formatDanishDate)
import Siteelm.DesignSystem exposing (PillType(..), pill)
import Siteelm.Html as Html
import Siteelm.Html.Attributes exposing (content, property)
import Siteelm.Page exposing (Page, page)
import Static.View as View
import Time exposing (Posix)


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
    , path : String
    , originalUrl : Maybe String
    , category : List String
    }


preambleDecoder : Decoder Preamble
preambleDecoder =
    D.map5 Preamble
        (D.field "title" D.string)
        (D.field "date" datetime)
        (D.field "path" D.string)
        (D.field "original_url" D.string |> D.maybe)
        (D.field "category" <| D.list D.string)


viewHead : Preamble -> String -> List (Html Never)
viewHead preamble _ =
    let
        title =
            preamble.title ++ " | text.hmsk.me"

        url =
            "https://text.hmsk.me/entries" ++ preamble.path
    in
    [ Html.title [] title
    , Html.script "https://cdn.iframe.ly/embed.js" ""
    , Html.meta [ property "og:title", content title ]
    , Html.meta [ property "og:url", content url ]
    , toUnstyled articleStyle
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
        , article []
            [ h2 [] [ text preamble.title ]
            , h3 [] [ text <| formatDanishDate preamble.date ]
            , div [ class "content pills" ] <|
                List.map
                    (\c -> pill Normal [] [ text c ])
                    preamble.category
            , linkForOriginal preamble.originalUrl
            , fromUnstyled <| Markdown.toHtmlWith markedOptions [] processedBody
            ]
        , View.footer
        ]


articleStyle : Html.Styled.Html msg
articleStyle =
    global
        [ typeSelector "article"
            [ descendants
                [ typeSelector "p, ul, h1, h2, h3, h4, table, blockquote, .content"
                    [ maxWidth (px 680)
                    , lineHeight (num 1.8)
                    ]
                , typeSelector "a"
                    [ Css.property "word-break" "break-all"
                    ]
                , typeSelector "pre"
                    [ backgroundColor <| hex "#fff"
                    , padding2 (rem 1.5) (px 0)
                    , lineHeight (num 1.5)
                    , overflow auto
                    , children
                        [ typeSelector "code"
                            [ display block
                            ]
                        ]
                    ]
                , typeSelector "blockquote"
                    [ backgroundColor <| hex "#fff"
                    , borderLeft3 (px 8) solid (hex "#397A9D")
                    , color (hex "#397A9D")
                    , padding2 (rem 0.5) (rem 1.0)
                    , lineHeight (num 1.5)
                    , children
                        [ typeSelector "p"
                            [ margin (px 0) ]
                        ]
                    ]
                , mediaQuery [ "screen and (min-width: 680px)" ]
                    [ typeSelector "p, ul, h1, h2, h3, h4, table, blockquote, .content"
                        [ margin3 (rem 1.5) auto (px 0)
                        ]
                    , typeSelector "pre"
                        [ children
                            [ typeSelector "code"
                                [ maxWidth (px 680)
                                , margin2 (px 0) auto
                                ]
                            ]
                        ]
                    ]
                , mediaQuery [ "screen and (max-width: 680px)" ]
                    [ typeSelector "p, ul, h1, h2, h3, h4, table, blockquote, .content"
                        [ margin3 (rem 1.5) (rem 1.5) (px 0)
                        ]
                    , typeSelector "pre"
                        [ children
                            [ typeSelector "code"
                                [ margin2 (px 0) (rem 1.5)
                                ]
                            ]
                        ]
                    ]
                , typeSelector "p"
                    [ children
                        [ typeSelector "code"
                            [ backgroundColor <| hex "#fff"
                            , padding (px 4)
                            , Css.property "word-break" "break-all"
                            ]
                        ]
                    , descendants
                        [ typeSelector "img"
                            [ width (pct 100)
                            ]
                        ]
                    ]
                ]
            ]
        ]


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
                    ( "\\[asin:(.+)\\](\\n|$)", amazon )

                Instagram ->
                    ( "\\[instagram:(.+)\\](\\n|$)", instagram )

                EmbedCard ->
                    ( "\\[embed:(.+)\\](\\n|$)", embedCard )
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
                    case String.split ":" b of
                        [ asin, title ] ->
                            "<div data-elm-module=\"Dynamic.Amazon\" data-flags=\"{ asin: '" ++ asin ++ "', title: '" ++ title ++ "'}\"></div>\n"

                        _ ->
                            "Format Error for Dynamic.Amazon"

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
                    "<div data-elm-module=\"Dynamic.Instagram\" data-flags=\"{ id: '" ++ b ++ "'}\"></div>\n"

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
                    "<div data-elm-module=\"Dynamic.EmbedCard\" data-flags=\"{ url: '" ++ b ++ "', embed: '' }\"></div>\n"

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
