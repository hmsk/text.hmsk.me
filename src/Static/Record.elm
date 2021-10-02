module Static.Record exposing (main)

import Css
    exposing
        ( absolute
        , auto
        , backgroundColor
        , block
        , borderLeft3
        , borderRadius
        , bottom
        , color
        , column
        , cursor
        , display
        , displayFlex
        , flexDirection
        , fontSize
        , hex
        , lineHeight
        , margin
        , margin2
        , margin3
        , marginBottom
        , marginLeft
        , marginTop
        , maxWidth
        , none
        , num
        , overflow
        , padding
        , padding2
        , pct
        , pointer
        , position
        , px
        , relative
        , rem
        , solid
        , vw
        , width
        )
import Css.Global exposing (children, descendants, global, mediaQuery, typeSelector)
import Html exposing (Html)
import Html.Styled exposing (a, article, div, h2, p, span, text, toUnstyled)
import Html.Styled.Attributes exposing (class, href, target)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra exposing (datetime)
import Regex
import Siteelm.Date exposing (formatHyphenatedDate)
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
    { category : String
    , logs : List Log
    }


type alias Log =
    { title : String
    , date : Posix
    , type_ : String
    , url : Maybe String
    , comment : Maybe String
    }


logDecoder : Decoder Log
logDecoder =
    D.map5 Log
        (D.field "title" D.string)
        (D.field "date" datetime)
        (D.field "type" D.string)
        (D.field "url" D.string |> D.maybe)
        (D.field "comment" D.string |> D.maybe)


preambleDecoder : Decoder Preamble
preambleDecoder =
    D.map2 Preamble
        (D.field "category" D.string)
        (D.field "logs" (D.list logDecoder))


viewHead : Preamble -> String -> List (Html Never)
viewHead preamble _ =
    let
        title =
            "records/" ++ preamble.category ++ " | text.hmsk.me"

        url =
            "https://text.hmsk.me/records/" ++ preamble.category
    in
    [ Html.title [] title
    , Html.meta [ property "og:title", content title ]
    , Html.meta [ property "og:url", content url ]
    , toUnstyled articleStyle
    ]


viewBody : Preamble -> String -> List (Html Never)
viewBody preamble body =
    List.map
        toUnstyled
        [ View.header
        , article []
            [ h2 [] [ text <| "records/" ++ preamble.category ]
            , p [] [ text body ]
            , div [ class <| "content tableIsh" ++ preamble.category ] <| List.map extractLog preamble.logs
            ]
        , View.footer
        ]


extractLog : Log -> Html.Styled.Html msg
extractLog log =
    div [ class "tableIsh-row" ]
        [ div [ class "tableIsh-row-left" ]
            [ div []
                [ pill Inverse [] [ text <| formatHyphenatedDate log.date ]
                ]
            , div []
                [ pill Normal [] [ text log.type_ ]
                ]
            ]
        , div [] <| titleWithLink log :: commentIfExists log
        ]


titleWithLink : Log -> Html.Styled.Html msg
titleWithLink { url, title } =
    case url of
        Just link ->
            let
                decoratedLink =
                    if Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "amazon.co.jp/") link then
                        link ++ "/ref=nosim?tag=libe-22"

                    else
                        link
            in
            a [ href decoratedLink, target "_blank" ] [ text title ]

        _ ->
            text title


commentIfExists : Log -> List (Html.Styled.Html msg)
commentIfExists { comment } =
    case comment of
        Just theComment ->
            [ span [ class "comment" ]
                [ text "💬"
                , div [ class "comment-content" ] [ text theComment ]
                ]
            ]

        _ ->
            []


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

                -- For Record.elm only start
                , typeSelector "div.tableIsh"
                    []
                , typeSelector "div.tableIsh-row"
                    [ displayFlex ]
                , typeSelector "div.tableIsh-row + div.tableIsh-row"
                    [ marginTop (px 16) ]
                , typeSelector "div.tableIsh-row-left"
                    [ displayFlex, width (px 180) ]
                , typeSelector "div.tableIsh-row-left > div + div"
                    [ margin2 (px 0) (px 8)
                    ]
                , typeSelector "div.tableIsh-row .comment"
                    [ marginLeft (px 8)
                    , cursor pointer
                    , position relative
                    ]
                , typeSelector "div.tableIsh-row .comment:hover > .comment-content"
                    [ display block ]
                , typeSelector "div.tableIsh-row .comment-content"
                    [ display none
                    , position absolute
                    , backgroundColor (hex "#397a9dee")
                    , color (hex "#fff")
                    , borderRadius (px 12)
                    , padding (px 12)
                    , fontSize (rem 0.8)
                    , bottom (px 24)
                    ]
                , mediaQuery [ "screen and (max-width: 680px)" ]
                    [ typeSelector "div.tableIsh-row" [ flexDirection column ]
                    , typeSelector "div.tableIsh-row-left" [ marginBottom (px 4) ]
                    , typeSelector "div.tableIsh-row .comment-content" [ width (vw 80) ]
                    ]

                -- For Record.elm only end
                ]
            ]
        ]
