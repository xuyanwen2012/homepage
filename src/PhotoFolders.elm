module PhotoFolders exposing (Model, Msg, init, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)



-- Constants


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"



-- Json


type alias JsonPhoto =
    { title : String
    , size : Int
    , relatedUrls : List String
    }


jsonDecoder : Decoder JsonPhoto
jsonDecoder =
    Decode.succeed JsonPhoto
        |> required "title" string
        |> required "size" int
        |> required "related_photos" (list string)


{-| Our decoder decodes the "photos" field of Folder
-}
photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decode.keyValuePairs jsonDecoder
        |> Decode.map fromPairs


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" string
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list folderDecoder))


modelPhotoDecoder : Decoder (Dict String Photo)
modelPhotoDecoder =
    Decode.succeed modelPhotoFromJson
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list modelPhotoDecoder))


folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name = name
        , expanded = True
        , photoUrls = Dict.keys photos
        , subfolders = subfolders
        }


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url = url
      , size = json.size
      , title = json.title
      , relatedUrls = json.relatedUrls
      }
    )


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs pairs =
    pairs
        |> List.map finishPhoto
        |> Dict.fromList


modelPhotoFromJson :
    Dict String Photo
    -> List (Dict String Photo)
    -> Dict String Photo
modelPhotoFromJson folderPhotos subfolderPhotos =
    List.foldl Dict.union folderPhotos subfolderPhotos



-- Models


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


type alias Model =
    { selectedUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


type Folder
    = Folder
        { name : String
        , expanded : Bool
        , photoUrls : List String
        , subfolders : List Folder
        }


type FolderPath
    = End
    | Subfolder Int FolderPath


initialModel : Model
initialModel =
    { selectedUrl = Nothing
    , photos = Dict.empty
    , root =
        Folder
            { name = "Loading.."
            , expanded = True
            , photoUrls = []
            , subfolders = []
            }
    }


init : Maybe String -> ( Model, Cmd Msg )
init selectedFilename =
    ( { initialModel | selectedUrl = selectedFilename }
    , Http.get
        { url = urlPrefix ++ "folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )



-- Msg


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)
    | ClickedFolder FolderPath


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\photos root ->
            { photos = photos, root = root, selectedUrl = Nothing }
        )
        modelPhotoDecoder
        folderDecoder



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

        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }



-- View


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "folders" ]
            [ viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []


viewPhoto : String -> Html Msg
viewPhoto url =
    div [ class "photo", onClick (ClickedPhoto url) ]
        [ text url ]


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.append
                    (List.indexedMap viewSubfolder folder.subfolders)
                    (List.map viewPhoto folder.photoUrls)
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)
