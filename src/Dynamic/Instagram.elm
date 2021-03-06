module Dynamic.Instagram exposing (main)

import Browser
import Html exposing (Html, a, img, p)
import Html.Attributes exposing (class, href, src, target)


type alias Model =
    { id : String
    }


type Msg
    = Nothing


main : Program Model Model Msg
main =
    Browser.element
        { init = \flags -> ( flags, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        url =
            "https://instagram.com/p/" ++ model.id ++ "/"

        imageUrl =
            "/uploads/" ++ model.id ++ ".jpg"
    in
    p []
        [ a
            [ href url
            , target "_blank"
            , class "dynamicInstagram"
            ]
            [ img
                [ src imageUrl
                ]
                []
            ]
        ]
