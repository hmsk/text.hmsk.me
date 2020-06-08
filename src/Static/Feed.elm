module Static.Feed exposing (main)

import Date exposing (fromCalendarDate)
import Html exposing (Html, text)
import Json.Decode as D exposing (Decoder)
import Rss exposing (Item, generate)
import Siteelm.Page exposing (Page, page)
import Time exposing (Month(..), millisToPosix)


main : Page Preamble
main =
    page
        { decoder = preambleDecoder
        , head = viewHead
        , body = viewBody
        }


type alias Preamble =
    { entries : List Entry
    }


type alias Entry =
    { url : String
    , title : String
    , date : String
    }


preambleDecoder : Decoder Preamble
preambleDecoder =
    D.map Preamble
        (D.field "entries" (D.list entryDecoder))


entryDecoder : Decoder Entry
entryDecoder =
    D.map3 Entry
        (D.field "url" D.string)
        (D.field "title" D.string)
        (D.field "date" D.string)


viewHead : Preamble -> String -> List (Html Never)
viewHead _ _ =
    []


viewBody : Preamble -> String -> List (Html Never)
viewBody preamble _ =
    [ text <| rssify preamble.entries ]


rssify : List Entry -> String
rssify entries =
    generate
        { title = "text.hmsk.me"
        , description = "text hmsk wrote"
        , url = "https://text.hmsk.me/"
        , lastBuildTime = millisToPosix 1591597113890
        , generator = Just "japonica"
        , items = List.map itemify <| List.reverse entries
        , siteUrl = "https://text.hmsk.me/"
        }


itemify : Entry -> Item
itemify entry =
    { title = entry.title
    , description = entry.title
    , url = entry.url
    , categories = []
    , author = "Kengo Hamasaki"
    , pubDate = Rss.Date <| fromCalendarDate 2020 Jun 7
    , content = Nothing
    }
