module Static.Basic exposing (main)

import Html exposing (Html, a, div, h2, li, main_, text, ul)
import Html.Attributes exposing (class, href, name)
import Json.Decode as D exposing (Decoder)
import Markdown
import Siteelm.Html as Html
import Siteelm.Html.Attributes exposing (charset, content)
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
    [ Html.meta [ charset "utf-8" ]
    , Html.title [] preamble.title
    , Html.meta [ name "description", content "text.hmsk.me" ]
    ]


{-| Make contents inside the _body_ tag. The parameter "body" is usually something like markdown.
-}
viewBody : Preamble -> String -> List (Html Never)
viewBody preamble _ =
    [ View.header
    , main_
        []
        [ h2 [] [ text "All entries" ]
        , div [ class "inner" ]
            [ ul []
                (List.map
                    linkToEntry
                    (List.reverse preamble.entries)
                )
            ]
        ]
    , View.footer
    ]


linkToEntry : Entry -> Html Never
linkToEntry article =
    li []
        [ a [ href article.url ]
            [ text article.title
            ]
        ]
