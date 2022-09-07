module Static.View exposing (footer, header)

import Css exposing (borderStyle, center, color, fontSize, fontWeight, hex, int, marginTop, none, px, textAlign, textDecoration)
import Html.Styled exposing (Html, a, span, text)
import Html.Styled.Attributes exposing (css, href)


header : Html.Styled.Html Never
header =
    Html.Styled.header
        [ css
            [ textAlign center
            ]
        ]
        [ a
            [ href "/"
            , css
                [ textDecoration none
                , color <| hex "#597B8C"
                , fontWeight (int 500)
                , fontSize (px 24)
                , borderStyle none
                ]
            ]
            [ text "text.hmsk.me" ]
        ]


footer : Html Never
footer =
    Html.Styled.footer
        [ css
            [ textAlign center
            , marginTop (px 48)
            ]
        ]
        [ span [] [ text "© 2012-2022 " ]
        , a [ href "/more" ] [ text "@hmsk が書いています" ]
        ]
