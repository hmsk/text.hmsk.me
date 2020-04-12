module Dynamic.Instagram exposing (main)

import Browser
import Html exposing (Html, a, p, img, text)
import Html.Attributes exposing (href, target, src)


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
  (model, Cmd.none)


view : Model -> Html Msg
view model =
  let
      url = "https://instagram.com/p/" ++ model.id ++ "/ref=nosim?tag=libe-22"
      imageUrl = "https://instagram.com/p/" ++ model.id ++ "/media/?size=l"
  in

  p [] [
    a [
      href url,
      target "_blank"
    ] [
      img [
        src imageUrl
      ] []
    ]
  ]
