module Siteelm.DesignSystem exposing (PillType(..), pill, styled)

import Css exposing (Style, backgroundColor, borderRadius, color, hex, padding2, px)
import Html.Styled exposing (Attribute, Html, span)
import Html.Styled.Attributes exposing (css)


type PillType
    = Normal
    | Inverse


pill : PillType -> List Style -> List (Html msg) -> Html msg
pill pillType optionalStyle children =
    let
        ( bgColor, textColor ) =
            case pillType of
                Normal ->
                    ( hex "#FFF", hex "#397A9D" )

                Inverse ->
                    ( hex "#397A9D", hex "#FFF" )
    in
    styled span
        ([ backgroundColor bgColor
         , color textColor
         , borderRadius <| px 4
         , padding2 (px 2) (px 4)
         ]
            ++ optionalStyle
        )
        []
        children


styled :
    (List (Attribute a) -> List (Html b) -> Html msg)
    -> List Style
    -> List (Attribute a)
    -> List (Html b)
    -> Html msg
styled fn styles attrs children =
    fn (attrs ++ [ css styles ]) children
