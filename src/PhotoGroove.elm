module PhotoGroove exposing (hum)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : { photos : List { url : String }, selectedUrl : String } -> Html.Html msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map
                (\photo -> viewThumbnail model.selectedUrl photo)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> { url : String } -> Html.Html msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList
            [ ( "selected", selectedUrl == thumb.url )
            ]
        ]
        []



--if selectedUrl == thumb.url then
--    img
--        [ src (urlPrefix ++ thumb.url)
--        , class "selected"
--        ]
--        []
--
--else
--    img [ src (urlPrefix ++ thumb.url) ] []


initialModel : { photos : List { url : String }, selectedUrl : String }
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


hum =
    view initialModel
