module PhotoGroove exposing (hum)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"



---- MODEL ----


type alias Model =
    { photos : List { url : String }, selectedUrl : String }


init : Model
init =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }



---- UPDATE ----


type Msg
    = ImgClick String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ImgClick data ->
            { model | selectedUrl = data }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map
                (viewThumbnail model.selectedUrl)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> { url : String } -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList
            [ ( "selected", selectedUrl == thumb.url )
            ]
        , onClick (ImgClick thumb.url)
        ]
        []



---- PROGRAM ----


hum =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
