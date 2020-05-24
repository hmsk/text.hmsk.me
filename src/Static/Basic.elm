module Static.Basic exposing (main)

import Css exposing (..)
import Html exposing (Html)
import Html.Styled exposing (a, div, h2, li, main_, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (class, css, href)
import Json.Decode as D exposing (Decoder)
import Siteelm.Html as Html
import Siteelm.Html.Attributes exposing (content, property)
import Siteelm.Page exposing (Page, page)
import Static.View as View


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
    { url : String
    , title : String
    , date : String
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
    D.map3 Entry
        (D.field "url" D.string)
        (D.field "title" D.string)
        (D.field "date" D.string)


{-| Make contents inside the _head_ tag.
-}
viewHead : Preamble -> String -> List (Html Never)
viewHead preamble _ =
    [ Html.title [] preamble.title
    , Html.meta [ Siteelm.Html.Attributes.property "og:url", Siteelm.Html.Attributes.content "https://text.hmsk.me/" ]
    , Html.meta [ Siteelm.Html.Attributes.property "og:title", Siteelm.Html.Attributes.content "text.hmsk.me" ]
    ]


{-| Make contents inside the _body_ tag. The parameter "body" is usually something like markdown.
-}
viewBody : Preamble -> String -> List (Html Never)
viewBody preamble _ =
    List.map
        toUnstyled
        [ View.header
        , main_
            [ css
                [ maxWidth (px 1280)
                , margin auto
                ]
            ]
            [ h2 [ css [ fontWeight (int 500) ] ] [ text "All entries" ]
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


linkToEntry : Entry -> Html.Styled.Html Never
linkToEntry article =
    li [ css [ listStyle none, margin2 (px 8) (px 0) ] ]
        [ a [ href article.url ]
            [ text article.title
            ]
        ]
