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
    = FromPhotoFolders Pages.PhotoFolders.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromPhotoFolders newMsg ->
            let
                ( foldersModel, foldersCmd ) =
                    Pages.PhotoFolders.update newMsg model

            in
            ( { model |
                selectedPhotoUrl = foldersModel.selectedPhotoUrl
                , photos         = foldersModel.photos
                , root           = foldersModel.root
              }
            , Cmd.map FromPhotoFolders foldersCmd
            )


save : Model -> Shared.Model -> Shared.Model
save model shared =
    Tuple.first (Shared.update (Shared.UpdateFolders model) shared)


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

        folderElement : List (Element Pages.PhotoFolders.Msg) -> Element Pages.PhotoFolders.Msg
        folderElement list =
            case List.head list of
                 Just newElement ->
                     newElement

                 Nothing ->
                     el
                        []
                        ( text "Nothing here" )

        body : Element Msg
        body =
            Element.map FromPhotoFolders (folderElement folderView.body)

    in
    { title = folderView.title ++ " - " ++ title
    , body = [ body ]
    }