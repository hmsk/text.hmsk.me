module Static.Basic exposing (main)

import Html exposing (Html, a, div, h2, li, text, ul)
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
viewBody preamble body =
    [ View.header
    , div
        [ class "main" ]
        [ div []
            [ h2 [] [ text "Dynamic components" ]
            , div [ class "inner" ]
                [ text "with Browser.element, dynamic contents can be embedded"
                , Html.dynamic
                    { moduleName = "Dynamic.Counter"
                    , flags = "{value: 100}"
                    }
                ]
            ]
        , div []
            [ h2 [] [ text "All entries" ]
            , div [ class "inner" ]
                [ div []
                    [ text "Use \"preamblesIn\" parameter to get preambles of files in a specified directory."
                    ]
                , div []
                    [ text "Be aware that there are some parameters which Siteelm automatically sets in an preamble. At the moment, a property \"url\" is that."
                    ]
                , ul []
                    (List.map
                        linkToEntry
                        preamble.entries
                    )
                ]
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
