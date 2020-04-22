module PhotoGroove exposing (hum)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"



---- MODEL ----


type alias Model =
    { photos : List Photo, selectedUrl : String }


type alias Photo =
    { url : String }


init : Model
init =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


photoArray : Array Photo
photoArray =
    Array.fromList init.photos



---- UPDATE ----


type Msg
    = ImgClick String
    | ButtonClick


update : Msg -> Model -> Model
update msg model =
    case msg of
        ImgClick data ->
            { model | selectedUrl = data }

        ButtonClick ->
            { model | selectedUrl = "2.jpeg" }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ButtonClick ]
            [ text "Surprise  Me!" ]
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


viewThumbnail : String -> Photo -> Html Msg
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
