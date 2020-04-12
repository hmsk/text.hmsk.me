module Dynamic.Amazon exposing (main)

import Browser
import Html exposing (Html, a, p, text)
import Html.Attributes exposing (href, target)


type alias Model =
    { asin : String
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
            "http://www.amazon.co.jp/dp/" ++ model.asin ++ "/ref=nosim?tag=libe-22"
    in
    p []
        [ a
            [ href url
            , target "_blank"
            ]
            [ text "商品へのリンク"
            ]
        ]
