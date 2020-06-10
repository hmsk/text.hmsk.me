module Static.Feed exposing (main)

import Browser
import Html exposing (Html, node, text)
import Html.Attributes exposing (attribute, href, rel, type_)
import Json.Decode as D exposing (Decoder, decodeString)


main : Program Flags (Model Preamble) Never
main =
    Browser.document
        { init = \f -> ( decode preambleDecoder f, Cmd.none )
        , update = \_ m -> ( m, Cmd.none )
        , view = \m -> { title = "", body = viewBody m }
        , subscriptions = always Sub.none
        }


type alias Flags =
    { preamble : String
    , body : String
    }


type alias Model a =
    { preamble : Maybe a
    , body : String
    }


decode : Decoder a -> Flags -> Model a
decode decoder flags =
    let
        preamble =
            flags.preamble
                |> decodeString decoder
                |> Result.toMaybe
    in
    { preamble = preamble
    , body = flags.body
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


viewBody : Model Preamble -> List (Html Never)
viewBody model =
    case model.preamble of
        Just p ->
            [ atomFeedFor <| List.reverse p.entries ]

        _ ->
            [ text "Nothing" ]


atomFeedFor : List Entry -> Html msg
atomFeedFor entries =
    let
        latestUpdated =
            case List.head entries of
                Just e ->
                    e.date

                _ ->
                    ""
    in
    node "feed"
        [ attribute "xmlns" "http://www.w3.org/2005/Atom" ]
    <|
        List.foldr
            (::)
            (List.map asAtom entries)
            [ node "title" [] [ text "text.hmsk.me" ]
            , node "id" [] [ text "tag:text.hmsk.me,2020:/index" ]
            , node "link" [ href "https://text.hmsk.me/feed.xml", type_ "application/atom+xml", rel "self" ] []
            , node "link" [ href "https://text.hmsk.me/", type_ "text/html", rel "alternate" ] []
            , node "updated" [] [ text latestUpdated ]
            , node "author"
                []
                [ node "name" [] [ text "@hmsk - Kengo Hamasaki" ]
                ]
            , node "generator" [] [ text "Japonica" ]
            ]


asAtom : Entry -> Html msg
asAtom entry =
    node "entry"
        []
        [ node "title" [] [ text entry.title ]
        , node "id" [] [ text <| "tag:text.hmsk.me,2020:" ++ entry.url ]
        , node "link" [ href <| "https://text.hmsk.me/" ++ entry.url, type_ "text/html", rel "alternate" ] []
        , node "updated" [] [ text entry.date ]
        , node "content" [] [ text <| "Content for " ++ entry.title ]
        ]
