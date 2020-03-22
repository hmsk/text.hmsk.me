module Static.View exposing (header, footer)

import Html exposing (Html, text, a)
import Html.Attributes exposing (href)

header : Html Never
header =
    Html.header [] [
        a [ href "/" ] [ text "text.hmsk.me" ]
    ]

footer : Html Never
footer =
    Html.footer [] [ text "© 2012-2020 Kengo Hamasaki / @hmsk" ]
