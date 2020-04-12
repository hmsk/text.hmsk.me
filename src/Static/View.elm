module Static.View exposing (footer, header)

import Html exposing (Html, a, text)
import Html.Attributes exposing (href)


header : Html Never
header =
    Html.header []
        [ a [ href "/" ] [ text "text.hmsk.me" ]
        ]


footer : Html Never
footer =
    Html.footer [] [ text "© 2012-2020 Kengo Hamasaki / @hmsk" ]
