module Api.Folder exposing (Folder (..), Model, initialModel, modelDecoder)

import Api.Photo            exposing (Photo)
import Dict                 exposing (Dict)
import Json.Decode          as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos           : Dict String Photo
    , root             : Folder
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


type Folder
    = Folder
        { name       : String
        , photoUrls  : List String
        , subfolders : List Folder
        , expanded   : Bool
        }


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


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\photos root ->
            { photos = photos, root = root, selectedPhotoUrl = Nothing }
        )
        modelPhotosDecoder
        folderDecoder