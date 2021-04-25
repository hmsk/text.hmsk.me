module Dynamic.Amazon exposing (main)

import Browser
import Css exposing (alignItems, backgroundColor, block, bold, border3, center, display, displayFlex, dotted, fontWeight, height, hex, hover, justifyContent, marginBottom, none, paddingLeft, paddingRight, px, solid, spaceBetween, textDecoration)
import Html exposing (Html)
import Html.Styled exposing (a, div, img, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, href, src, target)


type alias Model =
    { asin : String
    , title : String
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
    toUnstyled <|
        a
            [ class "content"
            , href url
            , target "_blank"
            , css
                [ display block
                , textDecoration none
                , border3 (px 1) solid <| hex "#DEDEDE"
                , backgroundColor <| hex "#FFF"
                , height (px 140)
                , hover
                    [ border3 (px 1) dotted <| hex "#397A9D"
                    ]
                ]
            ]
            [ div
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , alignItems center
                    ]
                ]
                [ div
                    [ css
                        [ paddingLeft (px 16)
                        , paddingRight (px 16)
                        , fontWeight bold
                        ]
                    ]
                    [ img [ src "https://www.amazon.co.jp/favicon.ico", css [ display block, height (px 16), marginBottom (px 2) ] ] []
                    , text model.title
                    ]
                , div []
                    [ img
                        [ css
                            [ height (px 138)
                            ]
                        , src <| "//ws-fe.amazon-adsystem.com/widgets/q?&MarketPlace=JP&ASIN=" ++ model.asin ++ "&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL280_&tag=libe-22"
                        ]
                        []
                    ]
                ]
            ]
