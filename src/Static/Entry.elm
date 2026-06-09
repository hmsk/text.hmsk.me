module Static.Entry exposing (main)

import Css
    exposing
        ( auto
        , backgroundColor
        , block
        , bold
        , border3
        , borderLeft3
        , borderRadius
        , color
        , display
        , fontSize
        , fontWeight
        , height
        , hex
        , hidden
        , lineHeight
        , margin
        , margin2
        , margin3
        , marginTop
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
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Styled exposing (a, article, div, fromUnstyled, h2, h3, img, p, text, toUnstyled)
import Html.Styled.Attributes exposing (alt, class, css, href, src, target)
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
import Url


main : Page Preamble
main =
    page
        { decoder = preambleDecoder
        , head = viewHead
        , body = viewBody
        }


type alias EmbedData =
    { title : String
    , description : Maybe String
    , image : Maybe String
    }


type alias Preamble =
    { title : String
    , date : Posix
    , path : String
    , originalUrl : Maybe String
    , category : List String
    , embeds : Dict String EmbedData
    }


embedDataDecoder : Decoder EmbedData
embedDataDecoder =
    D.map3 EmbedData
        (D.field "title" D.string)
        (D.maybe <| D.field "description" D.string)
        (D.maybe <| D.field "image" D.string)


preambleDecoder : Decoder Preamble
preambleDecoder =
    D.map6 Preamble
        (D.field "title" D.string)
        (D.field "date" datetime)
        (D.field "path" D.string)
        (D.field "original_url" D.string |> D.maybe)
        (D.field "category" <| D.list D.string)
        (D.maybe (D.field "embeds" (D.dict embedDataDecoder)) |> D.map (Maybe.withDefault Dict.empty))


viewHead : Preamble -> String -> List (Html Never)
viewHead preamble _ =
    let
        title =
            preamble.title ++ " | text.hmsk.me"

        url =
            "https://text.hmsk.me/entries" ++ preamble.path
    in
    [ Html.title [] title
    , Html.meta [ property "og:title", content title ]
    , Html.meta [ property "og:url", content url ]
    , toUnstyled articleStyle
    ]


viewBody : Preamble -> String -> List (Html Never)
viewBody preamble body =
    let
        processedBody =
            body
                |> replacer "\\[asin:(.+)\\](\\n|$)" amazon
                |> replacer "\\[instagram:(.+)\\](\\n|$)" instagram

        segments =
            splitEmbedSegments preamble.embeds processedBody
    in
    List.map
        toUnstyled
        [ View.header
        , article []
            ([ h2 [] [ text preamble.title ]
             , h3 [] [ text <| formatDanishDate preamble.date ]
             , div [ class "content pills" ] <|
                List.map
                    (\c -> pill Normal [] [ text c ])
                    preamble.category
             , linkForOriginal preamble.originalUrl
             ]
                ++ List.map renderSegment segments
            )
        , View.footer
        ]


articleStyle : Html.Styled.Html msg
articleStyle =
    global
        [ typeSelector "article"
            [ descendants
                [ typeSelector "p, ul, h1, h2, h3, h4, table, blockquote, .content, .embed-card"
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
                    [ typeSelector "p, ul, h1, h2, h3, h4, table, blockquote, .content, .embed-card"
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
                    [ typeSelector "p, ul, h1, h2, h3, h4, table, blockquote, .content, .embed-card"
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


replacer : String -> (List (Maybe String) -> String) -> String -> String
replacer pattern handler original =
    case Regex.fromString pattern of
        Nothing ->
            original

        Just regex ->
            Regex.replace regex (.submatches >> handler) original


amazon : List (Maybe String) -> String
amazon list =
    case List.head list of
        Just (Just b) ->
            case String.split ":" b of
                [ asin, title ] ->
                    "<div data-elm-module=\"Dynamic.Amazon\" data-flags=\"{ asin: '" ++ asin ++ "', title: '" ++ title ++ "'}\"></div>\n"

                _ ->
                    "Format Error for Dynamic.Amazon"

        _ ->
            ""


instagram : List (Maybe String) -> String
instagram list =
    case List.head list of
        Just (Just b) ->
            "<div data-elm-module=\"Dynamic.Instagram\" data-flags=\"{ id: '" ++ b ++ "'}\"></div>\n"

        _ ->
            ""


type Segment
    = MarkdownSegment String
    | EmbedSegment String (Maybe EmbedData)


splitEmbedSegments : Dict String EmbedData -> String -> List Segment
splitEmbedSegments embeds body =
    case Regex.fromString "\\[embed:(.+?)\\](?:\\n|$)" of
        Nothing ->
            [ MarkdownSegment body ]

        Just regex ->
            buildSegments embeds body (Regex.find regex body) 0


buildSegments : Dict String EmbedData -> String -> List Regex.Match -> Int -> List Segment
buildSegments embeds body matches offset =
    case matches of
        [] ->
            let
                remaining =
                    String.dropLeft offset body
            in
            if String.isEmpty remaining then
                []

            else
                [ MarkdownSegment remaining ]

        match :: rest ->
            let
                before =
                    String.slice offset match.index body

                url =
                    match.submatches
                        |> List.head
                        |> Maybe.andThen identity
                        |> Maybe.withDefault ""

                beforeSegment =
                    if String.isEmpty (String.trim before) then
                        []

                    else
                        [ MarkdownSegment before ]
            in
            beforeSegment
                ++ [ EmbedSegment url (Dict.get url embeds) ]
                ++ buildSegments embeds body rest (match.index + String.length match.match)


renderSegment : Segment -> Html.Styled.Html Never
renderSegment segment =
    case segment of
        MarkdownSegment md ->
            fromUnstyled <| Markdown.toHtmlWith markedOptions [] md

        EmbedSegment url (Just data) ->
            viewEmbedCard url data

        EmbedSegment url Nothing ->
            p [] [ a [ href url, target "_blank" ] [ text url ] ]


viewEmbedCard : String -> EmbedData -> Html.Styled.Html Never
viewEmbedCard url data =
    let
        domain =
            case Url.fromString url of
                Just u ->
                    u.host

                Nothing ->
                    url
    in
    div
        [ class "embed-card"
        , css
            [ border3 (px 1) solid (hex "#dddddd")
            , borderRadius (px 8)
            , overflow hidden
            , backgroundColor (hex "#f9f9f9")
            ]
        ]
        [ a
            [ href url
            , target "_blank"
            , css
                [ Css.property "text-decoration" "none"
                , Css.property "color" "inherit"
                , Css.property "display" "flex"
                , Css.property "align-items" "stretch"
                ]
            ]
            ([ div
                [ css
                    [ Css.property "flex" "1"
                    , padding2 (px 12) (px 16)
                    , Css.property "min-width" "0"
                    , Css.property "display" "flex"
                    , Css.property "flex-direction" "column"
                    , Css.property "gap" "4px"
                    ]
                ]
                [ div
                    [ css
                        [ fontWeight bold
                        , fontSize (px 14)
                        , color (hex "#1a1a1a")
                        , overflow hidden
                        , Css.property "white-space" "nowrap"
                        , Css.property "text-overflow" "ellipsis"
                        ]
                    ]
                    [ text data.title ]
                , case data.description of
                    Just desc ->
                        div
                            [ css
                                [ fontSize (px 12)
                                , color (hex "#666666")
                                , overflow hidden
                                , Css.property "display" "-webkit-box"
                                , Css.property "-webkit-line-clamp" "2"
                                , Css.property "-webkit-box-orient" "vertical"
                                ]
                            ]
                            [ text desc ]

                    Nothing ->
                        text ""
                , div
                    [ css
                        [ fontSize (px 12)
                        , color (hex "#397A9D")
                        , marginTop auto
                        ]
                    ]
                    [ text domain ]
                ]
             ]
                ++ (case data.image of
                        Just imgUrl ->
                            [ div
                                [ css
                                    [ width (px 120)
                                    , Css.property "flex-shrink" "0"
                                    ]
                                ]
                                [ img
                                    [ src imgUrl
                                    , alt ""
                                    , css
                                        [ width (pct 100)
                                        , height (pct 100)
                                        , Css.property "object-fit" "cover"
                                        , display block
                                        ]
                                    ]
                                    []
                                ]
                            ]

                        Nothing ->
                            []
                   )
            )
        ]


markedOptions : Markdown.Options
markedOptions =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Nothing
    , sanitize = False
    , smartypants = False
    }
