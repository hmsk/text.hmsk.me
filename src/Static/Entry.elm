module Static.Entry exposing (main)

import List
import Html exposing (Html, div, h2, text, article)
import Json.Decode as D exposing (Decoder)
import Markdown
import Regex
import Siteelm.Html as Html
import Siteelm.Html.Attributes exposing (charset)
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
    [ Html.meta [ charset "utf-8" ]
    , Html.title [] (preamble.title ++ " | sub")
    ]


viewBody : Preamble -> String -> List (Html Never)
viewBody preamble body =
    let
        processedBody = replaceCustomTag body
    in
    [ View.header
    , article []
        [ h2 [] [ text preamble.title ]
        , div []
            [ Markdown.toHtmlWith markedOptions [] processedBody
            ]
        ]
    , View.footer
    ]

replaceCustomTag : String -> String
replaceCustomTag original =
  case Regex.fromString "\\[asin:(.+):detail\\]" of
    Nothing ->
      original

    Just regex ->
      Regex.replace
        regex
        (.submatches >> amazon)
        original

amazon : List (Maybe String) -> String
amazon list = case List.head list of
   Just a -> case a of
      Just b -> "<div data-elm-module=\"Dynamic.Amazon\" data-flags=\"{ asin: '" ++ b ++ "'}\"></div>"
      _ -> "Nothing"
   _ -> "Nothing"

markedOptions : Markdown.Options
markedOptions =
  { githubFlavored = Just { tables = False, breaks = False }
  , defaultHighlighting = Nothing
  , sanitize = False
  , smartypants = False
  }
