port module Api.Gallery exposing
    ( Model
    , initialModel
    , ThumbnailSize(..)
    , Status(..)
    , sizeToString
    , activityChanges
    , setFilters
    , photoDecoder
    )

import Api.Photo            exposing (Photo)
import Json.Decode          exposing (Decoder, at, string, int, float, list, succeed)
import Json.Decode.Pipeline exposing (optional, required)



type alias Model =
    { status     : Status
    , activity   : String
    , chosenSize : ThumbnailSize
    , hue        : Float
    , ripple     : Float
    , noise      : Float
    }


initialModel : Model
initialModel =
    { status     = Loading
    , activity   = ""
    , chosenSize = Medium
    , hue        = 5
    , ripple     = 5
    , noise      = 5
    }


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


type Status
    = Loading
    | Loaded  (List Photo) String
    | Errored String


type ThumbnailSize
    = Small
    | Medium
    | Large


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg


type alias FilterOptions =
    { url     : String
    , filters : List { name : String, amount : Float }
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> optional "title"       string        "(untitled)"
        |> required "url"         string
        |> required "size"        int
        |> optional "requiredUrl" (list string) []