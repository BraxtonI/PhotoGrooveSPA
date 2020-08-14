module Pages.PhotoGallery exposing (Params, Model, Msg, page)

import Api.Photo            exposing (Photo)
import Element              exposing (..)
import Element.Events       as Events
import Element.Input        as Input
import Html                 exposing (Html, div, canvas, label, input, node)
import Html.Attributes      as Attr exposing (checked, id, name, type_)
import Html.Events          exposing (on, onClick)
import Http
import Json.Decode          exposing (Decoder, at, string, int, float, list, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Random
import Shared           exposing (urlPrefix)
import Spa.Document     exposing (Document)
import Spa.Page         as Page exposing (Page)
import Spa.Url          as Url exposing (Url)
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
    {}


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
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
    { title = "Photo Gallery"
    , body = []
    }