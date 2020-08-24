port module Api.LocalState exposing (saveState, decodeState)

import Shared
import Json.Decode          as Decode  exposing (Decoder)
import Json.Decode.Pipeline            exposing (hardcoded, required)
import Json.Encode          as Encode
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
    {-- If you want to include URL in the saved state, uncomment this.
    -- Make sure you also build the decoder in the next function as well.
    let
        url =
            case model.selectedPhotoUrl of
                Just photoUrl ->
                    photoUrl

                Nothing ->
                    ""

    in--}
    Encode.object
        [ --( "selectedPhotoUrl", Encode.string url )
        ( "root", encodeFolderHierarchy model.root )
        ]


decodeFolders : Folder.Model -> Decoder Folder.Model
decodeFolders folders =
    Decode.succeed Folder.Model
        --|> required "selectedPhotoUrl" Decode.string
        |> hardcoded folders.selectedPhotoUrl
        |> hardcoded folders.photos
        |> required "root" (decodeFolderRoot folders.root)


encodeFolderHierarchy : Folder -> Encode.Value
encodeFolderHierarchy (Folder folder) =
    Encode.object
        [ ( "expanded", Encode.bool folder.expanded )
        , ( "subfolders", Encode.list encodeFolderHierarchy folder.subfolders )
        ]


decodeFolderRoot : Folder -> Decoder Folder
decodeFolderRoot (Folder folder) =
    Decode.succeed folderFromJson
        |> hardcoded folder.name
        |> hardcoded folder.photoUrls
        |> hardcoded folder.subfolders
        --|> required "subfolders" Decode.list (decodeFolderList folder.subfolders)
        |> required "expanded" Decode.bool


decodeFolderList : List Folder -> List (Decoder Folder)
decodeFolderList folders =
    List.map decodeFolderRoot folders


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