module PhotoGroove exposing (hum)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type ThumbnailSize
    = Small
    | Medium
    | Large



---- MODEL ----


type alias Model =
    { photos : List Photo, selectedUrl : String, chosenSize : ThumbnailSize }


{-| Photo Item is represented by a string of the filename
-}
type alias Photo =
    { url : String }


init : ( Model, Cmd Msg )
init =
    ( { photos =
            [ { url = "1.jpeg" }
            , { url = "2.jpeg" }
            , { url = "3.jpeg" }
            ]
      , selectedUrl = "1.jpeg"
      , chosenSize = Medium
      }
    , Cmd.none
    )


photoArray : Array Photo
photoArray =
    --Array.fromList init.photos
    Array.fromList (Tuple.first init).photos



---- UPDATE ----


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotSelectedIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )


getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise  Me!" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])

        --[ viewSizeChooser Small (model.selectedUrl == sizeToString Small)
        --, viewSizeChooser Medium (model.selectedUrl == sizeToString Medium)
        --, viewSizeChooser Large (model.selectedUrl == sizeToString Large)
        --]
        , div [ id "thumbnails", class (sizeToClass model.chosenSize) ]
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
        , onClick (ClickedPhoto thumb.url)
        ]
        []



--viewSizeChooser : ThumbnailSize -> Bool -> Html Msg
--viewSizeChooser size isChecked =


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ClickedSize size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"



---- PROGRAM ----


hum : Program () Model Msg
hum =
    Browser.element
        --{ init = \_ -> ( init, Cmd.none )
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
