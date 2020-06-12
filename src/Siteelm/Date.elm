module Siteelm.Date exposing (formatDanishDate, formatHyphenatedDate)

import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)


formatDanishDate : Posix -> String
formatDanishDate posix =
    let
        y =
            String.fromInt <| toYear utc posix

        m =
            toFullDanishMonth <| toMonth utc posix

        d =
            String.fromInt <| toDay utc posix
    in
    String.concat [ m, " ", d, ", ", y ]


formatHyphenatedDate : Posix -> String
formatHyphenatedDate posix =
    let
        y =
            String.fromInt <| toYear utc posix

        m =
            toTwoDigitsMonth <| toMonth utc posix

        d =
            String.padLeft 2 '0' <| String.fromInt <| toDay utc posix
    in
    String.concat [ y, "-", m, "-", d ]


toFullDanishMonth : Month -> String
toFullDanishMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "Feburary"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


toTwoDigitsMonth : Month -> String
toTwoDigitsMonth month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"
