module Static.Index exposing (main)

import Css exposing (auto, em, fontSize, fontWeight, int, lineHeight, listStyle, margin, margin2, margin3, marginLeft, marginTop, maxWidth, none, num, padding, px)
import Css.Global exposing (children, descendants, global, mediaQuery, typeSelector)
import Html exposing (Html)
import Html.Styled exposing (a, div, h2, li, main_, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (class, css, href)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra exposing (datetime)
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


{-| Preamble is what you write on the head of the content files.
-}
type alias Preamble =
    { title : String
    , entries : List Entry
    }


type alias Entry =
    { path : String
    , title : String
    , date : Posix
    , category : List String
    }


{-| Preamble is passed as a JSON string. So it requires a decoder.
-}
preambleDecoder : Decoder Preamble
preambleDecoder =
    D.map2 Preamble
        (D.field "title" D.string)
        (D.field "entries" (D.list entryDecoder))


entryDecoder : Decoder Entry
entryDecoder =
    D.map4 Entry
        (D.field "path" D.string)
        (D.field "title" D.string)
        (D.field "date" datetime)
        (D.field "category" <| D.list D.string)


{-| Make contents inside the _head_ tag.
-}
viewHead : Preamble -> String -> List (Html Never)
viewHead preamble _ =
    [ Html.title [] preamble.title
    , Html.meta [ Siteelm.Html.Attributes.property "og:url", Siteelm.Html.Attributes.content "https://text.hmsk.me/" ]
    , Html.meta [ Siteelm.Html.Attributes.property "og:title", Siteelm.Html.Attributes.content "text.hmsk.me" ]
    , toUnstyled mainStyle
    ]


{-| Make contents inside the _body_ tag. The parameter "body" is usually something like markdown.
-}
viewBody : Preamble -> String -> List (Html Never)
viewBody preamble _ =
    List.map
        toUnstyled
        [ View.header
        , main_ []
            [ h2 [] [ text "All entries" ]
            , div [ class "inner" ]
                [ ul [ css [ padding (px 0) ] ]
                    (List.map
                        linkToEntry
                        (List.reverse preamble.entries)
                    )
                ]
            ]
        , View.footer
        ]


mainStyle : Html.Styled.Html msg
mainStyle =
    global
        [ typeSelector "main"
            [ children
                [ typeSelector "h2" [ fontWeight (int 500) ]
                ]
            , descendants [ typeSelector "a" [ lineHeight (num 1.5) ] ]
            ]
        , mediaQuery [ "screen and (min-width: 680px)" ]
            [ typeSelector "main"
                [ maxWidth (px 680)
                , margin auto
                ]
            ]
        , mediaQuery [ "screen and (max-width: 680px)" ]
            [ typeSelector "main"
                [ margin3 (em 1.5) (px 40) (px 0)
                ]
            ]
        ]


linkToEntry : Entry -> Html.Styled.Html Never
linkToEntry article =
    li [ css [ listStyle none, margin2 (px 32) (px 0) ] ]
        [ div []
            [ categoryPills article
            , div [ css [ marginTop <| px 8 ] ]
                [ a
                    [ href article.path, css [ fontSize <| em 1.25 ] ]
                    [ text article.title ]
                ]
            ]
        ]


categoryPills : Entry -> Html.Styled.Html Never
categoryPills article =
    div
        [ css []
        ]
    <|
        [ pill Inverse [] [ text (formatHyphenatedDate article.date) ] ]
            ++ List.map
                (\c -> pill Normal [ marginLeft <| px 8 ] [ text c ])
                article.category
