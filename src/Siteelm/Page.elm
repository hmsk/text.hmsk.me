module Siteelm.Page exposing (Page, page)

import Browser
import Css exposing (auto, backgroundColor, baseline, bolder, borderBottom3, borderBox, borderStyle, bottom, boxSizing, color, dashed, display, dotted, em, fontFamilies, fontFamily, fontSize, fontWeight, height, hex, hover, inherit, lineHeight, listItem, margin, none, num, outline, outline3, outlineOffset, padding, padding3, pct, position, px, relative, solid, textDecoration, textDecoration2, textTransform, top, underline, verticalAlign)
import Css.Global exposing (Snippet, global, selector)
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
                        , Html.link [ href "/icons/icon-16.png", rel "icon", type_ "image/png", attribute "sizes" "16x16" ]
                        , Html.link [ href "/icons/icon-32.png", rel "icon", type_ "image/png", attribute "sizes" "32x32" ]
                        , Html.link [ href "/icons/icon-96.png", rel "icon", type_ "image/png", attribute "sizes" "96x96" ]
                        , Html.link [ href "/icons/icon-192.png", rel "icon", type_ "image/png", attribute "sizes" "192x192" ]
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
            ++ modernNormalize


modernNormalize : List Snippet
modernNormalize =
    -- https://github.com/sindresorhus/modern-normalize
    [ selector "*, *::before, *::after" [ boxSizing borderBox ]
    , selector ":root"
        [ Css.property "-moz-tab-size" "4"
        , Css.property "tab-size" "4"
        ]
    , selector "html"
        [ lineHeight (num 1.15)
        , Css.property "-webkit-text-size-adjust" "100%"
        ]
    , selector "body"
        [ margin (px 0)
        , fontFamilies
            [ "system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif", "Apple Color Emoji", "Segoe UI Emoji" ]
        ]
    , selector "hr"
        [ height (px 0)
        ]
    , selector "abbr[title]"
        [ textDecoration2 underline dotted
        ]
    , selector "b, strong"
        [ fontWeight bolder
        ]
    , selector "code, kbd, samp, pre"
        [ fontFamilies [ "SFMono-Regular", "Consolas", "Liberation Mono", "Menlo", "monospace" ]
        , fontSize (em 1)
        ]
    , selector "small"
        [ fontSize (pct 80)
        ]
    , selector "sub,sup"
        [ fontSize (pct 75)
        , lineHeight (num 0)
        , position relative
        , verticalAlign baseline
        ]
    , selector "sub"
        [ bottom (em -0.25)
        ]
    , selector "sup"
        [ top (em -0.5)
        ]
    , selector "button, input, optgroup, select, textarea"
        [ fontFamily inherit
        , fontSize (pct 100)
        , lineHeight (num 1.15)
        , margin (px 0)
        ]
    , selector "button, select"
        [ textTransform none
        ]
    , selector "button, [type='button'], [type='reset'], [type='submit']"
        [ Css.property "-webkit-appearance" "button"
        ]
    , selector "button::-moz-focus-inner, [type='button']::-moz-focus-inner, [type='reset']::-moz-focus-inner, [type='submit']::-moz-focus-inner"
        [ borderStyle none
        , padding (px 0)
        ]
    , selector "button:-moz-focusring, [type='button']:-moz-focusring, [type='reset']:-moz-focusring, [type='submit']:-moz-focusring"
        [ Css.property "outline" "1px dotted ButtonText"
        ]
    , selector "fieldset"
        [ padding3 (em 0.35) (em 0.75) (em 0.625)
        ]
    , selector "legend"
        [ padding (px 0) ]
    , selector "progress"
        [ verticalAlign baseline ]
    , selector "[type='number']::-webkit-inner-spin-button, [type='number']::-webkit-outer-spin-button"
        [ height auto ]
    , selector "[type='search']"
        [ outlineOffset (px -2)
        , Css.property "-webkit-appearance" "textfield"
        ]
    , selector "[type='search']::-webkit-search-decoration"
        [ Css.property "-webkit-appearance" "none"
        ]
    , selector "::-webkit-file-upload-button"
        [ Css.property "font" "inherit"
        , Css.property "-webkit-appearance" "button"
        ]
    , selector "summary"
        [ display listItem
        ]
    ]



{- buttonText : ColorValue NonMixable
   buttonText =
   { value = "ButtonText"
   , color = Compatible
   }
-}
