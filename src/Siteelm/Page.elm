module Siteelm.Page exposing (Page, page)

import Browser
import Css exposing (backgroundColor, borderBottom3, borderStyle, color, dashed, em, fontFamilies, hex, hover, none, padding2, px, solid, textDecoration)
import Css.Global exposing (global, selector)
import Css.ModernNormalize as ModernNormalize
import Html exposing (Html)
import Html.Attributes exposing (attribute, href, lang, name, title, type_)
import Html.Styled exposing (Attribute, fromUnstyled, node, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Json.Decode exposing (Decoder, decodeString)
import Siteelm.Html as Html
import Siteelm.Html.Attributes exposing (charset, content, property, rel)


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
                    [ lang "ja" ]
                  <|
                    List.foldr
                        (::)
                        (head p model.body)
                        [ Html.meta [ charset "utf-8" ]
                        , Html.link [ href "https://fonts.googleapis.com/css2?family=M+PLUS+Rounded+1c:wght@400;500&display=swap", rel "stylesheet" ]
                        , Html.link [ href "https://text.hmsk.me/feed.xml", rel "alternate", type_ "application/atom+xml", title "Atom" ]
                        , Html.link [ href "/images/icons/icon-16.png", rel "icon", type_ "image/png", attribute "sizes" "16x16" ]
                        , Html.link [ href "/images/icons/icon-32.png", rel "icon", type_ "image/png", attribute "sizes" "32x32" ]
                        , Html.link [ href "/images/icons/icon-96.png", rel "icon", type_ "image/png", attribute "sizes" "96x96" ]
                        , Html.link [ href "/images/icons/icon-192.png", rel "icon", type_ "image/png", attribute "sizes" "192x192" ]
                        , Html.meta [ name "viewport", content "width=device-width,initial-scale=1,minimum-scale=1,maximum-scale=1,user-scalable=no" ]
                        , Html.meta [ name "author", content "@hmsk / Kengo Hamasaki" ]
                        , Html.meta [ name "theme-color", content "#597B8C" ]
                        , Html.meta [ name "description", property "og:description", content "text hmsk wrote" ]
                        , Html.meta [ property "og:image", content "https://text.hmsk.me/images/og_image.png" ]
                        , Html.meta [ property "og:type", content "blog" ]
                        , Html.meta [ property "twitter:card", content "summary" ]
                        , Html.meta [ property "twitter:creator", content "@hmsk" ]
                        , Html.meta [ property "twitter:site", content "@hmsk" ]
                        , toUnstyled globalStyle
                        , toUnstyled ModernNormalize.globalStyledHtml
                        ]
                , node "body" [ bodyStyle ] bodyHtml |> toUnstyled
                ]

        Nothing ->
            Html.text ""


bodyStyle : Attribute Never
bodyStyle =
    css
        [ fontFamilies [ "'M PLUS Rounded 1c'", "sans-serif" ]
        , color <| hex "#397A9D"
        , backgroundColor <| hex "#EAEEF0"
        , padding2 (em 2) (px 0)
        ]


globalStyle : Html.Styled.Html msg
globalStyle =
    global <|
        [ selector "a"
            [ color <| hex "#597B8C"
            , textDecoration none
            , borderBottom3 (px 1) solid <| hex "#597B8C"
            , hover
                [ textDecoration none
                , color <| hex "#397A9D"
                , borderBottom3 (px 1) dashed <| hex "#397A9D"
                ]
            ]
        , selector ".dynamicInstagram" [ borderStyle none ]
        ]
            ++ ModernNormalize.snippets
