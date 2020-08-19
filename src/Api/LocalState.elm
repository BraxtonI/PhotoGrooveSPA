port module Api.LocalState exposing (saveState, retrieveState)

import Shared
import Json.Encode as Encode
import Api.Folder  as Folder  exposing (Folder(..))
import Api.Gallery as Gallery exposing (ThumbnailSize(..), sizeToString)


port storeState : String -> Cmd msg


saveState : Shared.Model -> Cmd msg
saveState model =
    stateEncoder model
        |> Encode.encode 0
        |> storeState


retrieveState : Cmd msg
retrieveState =
    Cmd.none


stateEncoder : Shared.Model -> Encode.Value
stateEncoder model =
    Encode.object
        [ ( "foldersModel", encodeFolders model.foldersModel )
        , ( "galleryModel", encodeGallery model.galleryModel )
        ]


encodeFolders : Folder.Model -> Encode.Value
encodeFolders model =
    let
        url =
            case model.selectedPhotoUrl of
                Just photoUrl ->
                    photoUrl

                Nothing ->
                    ""

    in
    Encode.object
        [ ( "selectedPhotoUrl", Encode.string url )
        , ( "root", encodeFolderHierarchy model.root )
        ]


encodeFolderHierarchy : Folder -> Encode.Value
encodeFolderHierarchy (Folder folder) =
    Encode.object
        [ ( "name", Encode.string folder.name )
        , ( "expanded", Encode.bool folder.expanded )
        , ( "photoUrls", Encode.list Encode.string folder.photoUrls )
        , ( "subfolders", Encode.list encodeFolderHierarchy folder.subfolders )
        ]


encodeGallery : Gallery.Model -> Encode.Value
encodeGallery model =
    let
        size =
            sizeToString model.chosenSize

    in
    Encode.object
        [ ( "activity", Encode.string model.activity )
        , ( "chosenSize", Encode.string size )
        , ( "hue", Encode.float model.hue )
        , ( "ripple", Encode.float model.ripple )
        , ( "noise", Encode.float model.noise )
        ]