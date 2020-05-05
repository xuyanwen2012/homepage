module Main exposing (..)

import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import PhotoGroove



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm 2 App is working!" ]
        ]



---- PROGRAM ----
--main : Program () Model Msg


main =
    PhotoGroove.main



--
--main : Program () Model Msg
--main =
--    Browser.element
--        { view = view
--        , init = \_ -> init
--        , update = update
--        , subscriptions = always Sub.none
--        }
