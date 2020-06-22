module Static.Entry exposing (main)

import Css
    exposing
        ( auto
        , backgroundColor
        , block
        , display
        , em
        , hex
        , lineHeight
        , margin2
        , margin3
        , maxWidth
        , num
        , overflow
        , padding
        , padding2
        , pct
        , px
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
    , Html.meta [ Siteelm.Html.Attributes.property "og:title", Siteelm.Html.Attributes.content title ]
    , Html.meta [ Siteelm.Html.Attributes.property "og:url", Siteelm.Html.Attributes.content url ]
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
            , div [ class "pills" ] <|
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
                [ typeSelector "p, ul, h1, h2, h3, .pills"
                    [ maxWidth (px 680)
                    , lineHeight (num 1.8)
                    ]
                , typeSelector "a"
                    [ Css.property "word-break" "break-all"
                    ]
                , typeSelector "pre"
                    [ backgroundColor <| hex "#fff"
                    , padding2 (em 1.5) (px 0)
                    , lineHeight (num 1.5)
                    , overflow auto
                    , children
                        [ typeSelector "code"
                            [ display block
                            ]
                        ]
                    ]
                , mediaQuery [ "screen and (min-width: 680px)" ]
                    [ typeSelector "p, ul, h1, h2, h3, .pills"
                        [ margin3 (em 1.5) auto (px 0)
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
                    [ typeSelector "p, ul, h1, h2, h3, .pills"
                        [ margin3 (em 1.5) (px 40) (px 0)
                        ]
                    , typeSelector "pre"
                        [ children
                            [ typeSelector "code"
                                [ margin2 (px 0) (px 40)
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
