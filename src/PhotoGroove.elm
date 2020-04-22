module PhotoGroove exposing (hum)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : List { url : String } -> Html.Html msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ] (List.map viewThumbnail model)
        ]


viewThumbnail : { url : String } -> Html.Html msg
viewThumbnail thumb =
    img [ src (urlPrefix ++ thumb.url) ] []


initialModel : List { url : String }
initialModel =
    [ { url = "1.jpeg" }
    , { url = "2.jpeg" }
    , { url = "3.jpeg" }
    ]


hum =
    view initialModel
