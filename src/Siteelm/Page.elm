module Siteelm.Page exposing (Page, page)

import Browser
import Css exposing (backgroundColor, color, fontFamilies, hex)
import Html exposing (Html)
import Html.Attributes exposing (href, rel)
import Html.Styled exposing (Attribute, fromUnstyled, node, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Json.Decode exposing (Decoder, decodeString)
import Siteelm.Html as Html


{-| Generate a Program for static page. You need to give a decoder for your
preamble model and a view function which takes the preamble and
plain text body (e.g. markdown text).
-}
page :
    { decoder : Decoder a
    , head : a -> String -> List (Html Never)
    , body : a -> String -> List (Html Never)
    }
    -> Page a
page { decoder, head, body } =
    Browser.document
        { init = \f -> ( decode decoder f, Cmd.none )
        , update = \_ m -> ( m, Cmd.none )
        , view = \m -> { title = "", body = [ renderPage head body m ] }
        , subscriptions = always Sub.none
        }


type alias Page a =
    Program Flags (Model a) Never


type alias Model a =
    { preamble : Maybe a
    , body : String
    }


type alias Flags =
    { preamble : String
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


renderPage : (a -> String -> List (Html Never)) -> (a -> String -> List (Html Never)) -> Model a -> Html Never
renderPage head body model =
    case model.preamble of
        Just p ->
            let
                bodyHtml =
                    List.map fromUnstyled (body p model.body)
            in
            Html.html []
                [ Html.head
                    []
                  <|
                    List.concat
                        [ [ Html.link [ href "https://fonts.googleapis.com/css2?family=M+PLUS+Rounded+1c:wght@400;500&display=swap", rel "stylesheet" ] ]
                        , head p model.body
                        ]
                , toUnstyled <| node "body" [ bodyStyle ] bodyHtml
                ]

        Nothing ->
            Html.text ""


bodyStyle : Attribute Never
bodyStyle =
    css
        [ fontFamilies [ "'M PLUS Rounded 1c'", "sans-serif" ]
        , color <| hex "#397A9D"
        , backgroundColor <| hex "#EAEEF0"
        ]
