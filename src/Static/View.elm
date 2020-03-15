module Static.View exposing (header, footer)

import Html exposing (Html, text)

header : Html Never
header =
    Html.header [] [text "header comes here"]

footer : Html Never
footer =
    Html.footer [] [text "footer comes here"]
