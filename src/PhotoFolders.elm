module PhotoFolders exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)



-- Model


type alias Model =
    { selectedUrl : Maybe String
    }


initialModel : Model
initialModel =
    { selectedUrl = Nothing }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )



-- Msg


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)


modelDecoder =
    -- TODO: Temp
    Decode.succeed initialModel



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    h1 [] [ text "The Grooviest Folders the world has ever seen" ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
