port module Api.LocalState exposing (saveState, decodeState)

import Shared
import Json.Decode          as Decode  exposing (Decoder)
import Json.Decode.Pipeline            exposing (hardcoded, required, optional)
import Json.Encode          as Encode
import Json.Encode.Extra    as EncodeExtra
import Api.Folder           as Folder  exposing (Folder(..))
import Api.Gallery          as Gallery exposing (ThumbnailSize(..), sizeToString)


port storeState : String -> Cmd msg


saveState : Shared.Model -> Cmd msg
saveState model =
    encodeState model
        |> Encode.encode 0
        |> storeState


encodeState : Shared.Model -> Encode.Value
encodeState model =
    Encode.object
        [ ( "foldersModel", encodeFolders model.foldersModel )
        , ( "galleryModel", encodeGallery model.galleryModel )
        ]


decodeState : Shared.Model -> Decoder Shared.Model
decodeState shared =
    Decode.succeed (Shared.Model shared.url "state" shared.key shared.version)
        |> required "foldersModel" (decodeFolders shared.foldersModel)
        |> required "galleryModel" (decodeGallery shared.galleryModel)


encodeFolders : Folder.Model -> Encode.Value
encodeFolders model =
    Encode.object
        [ ( "selectedPhotoUrl", EncodeExtra.maybe Encode.string model.selectedPhotoUrl )
        , ( "root", encodeFolderHierarchy model.root )
        ]


decodeFolders : Folder.Model -> Decoder Folder.Model
decodeFolders model =
    Decode.succeed Folder.Model
        |> optional "selectedPhotoUrl" (Decode.map Just Decode.string) Nothing
        |> hardcoded model.photos
        |> required "root" decodeFolderRoot


encodeFolderHierarchy : Folder -> Encode.Value
encodeFolderHierarchy (Folder folder) =
    Encode.object
        [ ( "name", Encode.string folder.name )
        , ( "photoUrls", Encode.list Encode.string folder.photoUrls )
        , ( "subfolders", Encode.list encodeFolderHierarchy folder.subfolders )
        , ( "expanded", Encode.bool folder.expanded )
        ]


decodeFolderRoot : Decoder Folder
decodeFolderRoot =
    Decode.succeed folderFromJson
        |> required "name" Decode.string
        |> required "photoUrls" (Decode.list Decode.string)
        |> required "subfolders" (Decode.lazy (\_ -> Decode.list decodeFolderRoot))
        |> required "expanded" Decode.bool


folderFromJson : String -> List String -> List Folder -> Bool -> Folder
folderFromJson name photos subfolders expanded =
    Folder
        { name       = name
        , photoUrls  = photos
        , subfolders = subfolders
        , expanded   = expanded
        }


encodeGallery : Gallery.Model -> Encode.Value
encodeGallery model =
    let
        size =
            sizeToString model.chosenSize

    in
    Encode.object
        [ ( "activity", Encode.string model.activity )
        , ( "chosenSize", Encode.string size )
        , ( "chosenUrl", Encode.string model.chosenUrl )
        , ( "hue", Encode.float model.hue )
        , ( "ripple", Encode.float model.ripple )
        , ( "noise", Encode.float model.noise )
        ]


decodeGallery : Gallery.Model -> Decoder Gallery.Model
decodeGallery model =
    Decode.succeed (Gallery.Model model.status)
        |> required "activity" Decode.string
        |> required "chosenSize" sizeDecoder
        |> required "chosenUrl" Decode.string
        |> required "hue" Decode.float
        |> required "ripple" Decode.float
        |> required "noise" Decode.float


sizeDecoder : Decoder ThumbnailSize
sizeDecoder =
    Decode.map stringToSize Decode.string


stringToSize : String -> ThumbnailSize
stringToSize size =
    case size of
        "small" ->
            Small

        "med" ->
            Medium

        "large" ->
            Large

        _ ->
            Medium