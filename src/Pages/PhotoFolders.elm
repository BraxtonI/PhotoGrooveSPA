module Pages.PhotoFolders exposing (Params, Model, Msg, page)

import Dict                 exposing (Dict)
import Element              exposing (..)
import Element.Events       as Events
import Http
import Json.Decode          as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Shared               exposing (Photo, urlPrefix)
import Spa.Document         exposing (Document)
import Spa.Page             as Page exposing (Page)
import Spa.Url              as Url exposing (Url)
import UI


page : Page Params Model Msg
page =
    Page.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- INIT


type alias Params =
    ()


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos           : Dict String Photo
    , root             : Folder
    }


type Folder
    = Folder
        { name       : String
        , photoUrls  : List String
        , subfolders : List Folder
        , expanded   : Bool
        }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos           = Dict.empty
    , root             =
        Folder
            { name       = "Loading..."
            , expanded   = True
            , photoUrls  = []
            , subfolders = []
            }
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( { initialModel | selectedPhotoUrl = shared.foldersSelectedFilename }
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson LoadPage modelDecoder
        }
    )



-- UPDATE


type Msg
    = SelectPhotoUrl String
    | LoadPage       (Result Http.Error Model)
    | ToggleExpanded FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleExpanded path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )

        SelectPhotoUrl url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        LoadPage (Ok newModel) ->
            ( { newModel | selectedPhotoUrl = model.selectedPhotoUrl }, Cmd.none )

        LoadPage (Err _) ->
            ( model, Cmd.none )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Element Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    { title = "Photo Folders"
    , body =
        [ row
             UI.photoFoldersContent
             [ column
                 UI.folders
                 [ viewFolder End model.root ]
             , column
                 UI.selectedPhoto
                 [ selectedPhoto ]
             ]
        ]
    }


viewPhoto : String -> Element Msg
viewPhoto url =
    link
        ( UI.photo
        ++ [ Events.onClick (SelectPhotoUrl url) ]
        )
        { url = ("/photos/" ++ url)
        , label = text url }


viewSelectedPhoto : Photo -> Element Msg
viewSelectedPhoto photo =
    column
        UI.selectedPhoto
        [ el
            UI.h2
            ( text photo.title )
        , column
            []
            [ image
                UI.image
                { src = (urlPrefix ++ "photos/" ++ photo.url ++ "/full")
                , description = "Selected photo."
                }
            , el
                [centerX]
                ( text (String.fromInt photo.size ++ "KB") )
            ]
        , el
            UI.h3
            ( text "Related" )
        , row
            [ spacing 10 ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Element Msg
viewRelatedPhoto url =
    link
        (List.append UI.relatedPhoto [ Events.onClick (SelectPhotoUrl url) ])
        { url = ("/photos/" ++ url)
        , label =
            ( image
                []
                { src = (urlPrefix ++ "photos/" ++ url ++ "/thumb")
                , description = "Related photo."
                }
            )
        }


viewFolder : FolderPath -> Folder -> Element Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Element Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel : Bool -> Element Msg
        folderLabel expanded =
            el
                ( UI.folderLabel
                ++ [ Events.onClick (ToggleExpanded path) ]
                )
                ( text
                    (
                        ( if expanded then
                            "▸"
                          else
                            "▾"
                        )
                        ++ folder.name
                    )
                )
    in
    if folder.expanded then
        let
            contents =
                List.append
                    (List.indexedMap viewSubfolder folder.subfolders)
                    (List.map        viewPhoto     folder.photoUrls)
        in
        column
            ( UI.cascade
            )
            [ folderLabel True
            , column
                []
                contents
            ]
    else
        column
            UI.cascade
            [ folderLabel False ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)


type FolderPath
    = End
    | Subfolder Int FolderPath


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


type alias JsonPhoto =
    { title       : String
    , size        : Int
    , relatedUrls : List String
    }


jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decode.succeed JsonPhoto
        |> required "title"          string
        |> required "size"           int
        |> required "related_photos" (list string)


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url         = url
      , size        = json.size
      , title       = json.title
      , relatedUrls = json.relatedUrls
      }
    )


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs pairs =
    pairs
        |> List.map finishPhoto
        |> Dict.fromList


photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decode.keyValuePairs jsonPhotoDecoder
        |> Decode.map fromPairs


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" string
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list folderDecoder))


folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name       = name
        , expanded   = True
        , subfolders = subfolders
        , photoUrls  = Dict.keys photos
        }


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\photos root ->
            { photos = photos, root = root, selectedPhotoUrl = Nothing }
        )
        modelPhotosDecoder
        folderDecoder


modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decode.succeed modelPhotosFromJson
        |> required "photos"     photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list modelPhotosDecoder))


modelPhotosFromJson :
    Dict String Photo
    -> List (Dict String Photo)
    -> Dict String Photo
modelPhotosFromJson folderPhotos subfolderPhotos =
    List.foldl Dict.union folderPhotos subfolderPhotos