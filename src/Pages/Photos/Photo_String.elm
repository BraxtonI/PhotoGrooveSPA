module Pages.Photos.Photo_String exposing (Params, Model, Msg, page, init)

import Api.Folder           exposing (Model, initialModel)
import Element              exposing (..)
import Pages.PhotoFolders
import Shared
import Spa.Document         exposing (Document)
import Spa.Page             as Page exposing (Page)
import Spa.Url              as Url exposing (Url)


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
    { photo : String }


type alias Model =
    Api.Folder.Model


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    let
        ( model, cmds ) =
            (load shared Api.Folder.initialModel)

    in
    ( { model | selectedPhotoUrl = Just params.photo }, cmds )



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
    ( shared.foldersModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> Document Msg
view model =
    let
        folderView : Document Pages.PhotoFolders.Msg
        folderView =
            Pages.PhotoFolders.view model

        title =
            case model.selectedPhotoUrl of
                Just photoUrl ->
                    photoUrl

                Nothing ->
                    "Photo not found"

        body : List (Element Pages.PhotoFolders.Msg)
        body =
            folderView.body

    in
    { title = title
    , body =
       [ el
            []
            ( text ("Replace this with folder body. This should display " ++ title))
        }
    }