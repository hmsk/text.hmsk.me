module Siteelm.Html.Attributes exposing (charset, content, data, property, rel, role)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


content : String -> Attribute msg
content =
    attribute "content"


property : String -> Attribute msg
property =
    attribute "property"


charset : String -> Attribute msg
charset =
    attribute "charset"


rel : String -> Attribute msg
rel =
    attribute "rel"


role : String -> Attribute msg
role =
    attribute "role"


data : String -> String -> Attribute msg
data name value =
    attribute (String.join "-" [ "data", name ]) value
