module Dynamic.EmbedCard exposing (main)

import Browser
import Html exposing (Html, a, p, text)
import Html.Attributes exposing (href, target)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode exposing (Decoder, field, string)
import Url.Builder exposing (crossOrigin)


type alias Model =
    { url : String
    , embed : String
    }


type Msg
    = IframelyResponded (Result Http.Error String)


main : Program Model Model Msg
main =
    Browser.element
        { init = \flags -> ( { embed = "", url = flags.url }, getIframelyEmbedCode flags.url )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IframelyResponded result ->
            case result of
                Ok embedHtml ->
                    ( { model | embed = embedHtml }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        out =
            case model.embed of
                "" ->
                    [ text model.url ]

                _ ->
                    textHtml model.embed
    in
    p []
        [ a
            [ href model.url
            , target "_blank"
            ]
            out
        ]


textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []


getIframelyEmbedCode : String -> Cmd Msg
getIframelyEmbedCode url =
    Http.get
        { url =
            crossOrigin
                "https://iframe.ly"
                [ "api", "iframely" ]
                [ Url.Builder.int "omit_script" 1
                , Url.Builder.string "iframe" "card"
                , Url.Builder.string "url" url
                , Url.Builder.string "key" "0fce053df25e594d5d7a616362e565f2"
                ]
        , expect = Http.expectJson IframelyResponded iframeDecoder
        }


iframeDecoder : Decoder String
iframeDecoder =
    field "html" string
