module Main exposing (main)

import Api.LocalState
import Browser
import Browser.Navigation   as Nav
import Json.Decode          as Decode
import Shared               exposing (Flags)
import Spa.Document         as Document exposing (Document)
import Spa.Generated.Pages  as Pages
import Spa.Generated.Route  as Route exposing (Route)
import Url                  exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> Document.toBrowserDocument
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- INIT



type alias Flags =
    { version: Float
    , initState : Maybe String
    }


type alias Model =
    { shared : Shared.Model
    , page : Pages.Model
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        { version, initState } =
            flags

        ( shared, sharedCmd ) =
            Shared.init version url key

        ( page, pageCmd ) =
            Pages.init (fromUrl url) shared

        jsonString =
            case initState of
                Just newString ->
                    newString

                Nothing ->
                    ""

        loadedShared : Shared.Model
        loadedShared =
            case Decode.decodeString (Api.LocalState.decodeState shared) jsonString of
                Ok newState ->
                    Debug.log "The contents of the new state are: " newState

                Err _ ->
                    shared

    in
    ( Model loadedShared (Debug.log "page's contents are: " page)
    , Cmd.batch
        [ Cmd.map Shared (Debug.log "sharedCmd is: " sharedCmd)
        , Cmd.map Pages (Debug.log "pageCmd is: " pageCmd)
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Shared Shared.Msg
    | Pages Pages.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model
            , Nav.pushUrl model.shared.key (Url.toString url)
            )

        LinkClicked (Browser.External href) ->
            ( model
            , Nav.load href
            )

        UrlChanged url ->
            let
                original =
                    model.shared

                shared =
                    { original | url = url }

                ( page, pageCmd ) =
                    Pages.init (fromUrl url) shared
            in
            ( { model | page = page, shared = Pages.save page shared }
            , Cmd.map Pages pageCmd
            )

        Shared sharedMsg ->
            let
                ( shared, sharedCmd ) =
                    Shared.update sharedMsg model.shared

                ( page, pageCmd ) =
                    Pages.load model.page shared
            in
            ( { model | page = page, shared = shared }
            , Cmd.batch
                [ Cmd.map Shared sharedCmd
                , Cmd.map Pages pageCmd
                ]
            )

        Pages pageMsg ->
            let
                ( page, pageCmd ) =
                    Pages.update pageMsg model.page

                shared =
                    Pages.save page model.shared
            in
            ( { model | page = page, shared = shared }
            , Cmd.batch
                [ Cmd.map Pages pageCmd
                , Api.LocalState.saveState model.shared
                ]
            )


view : Model -> Document Msg
view model =
    Shared.view
        { page =
            Pages.view model.page
                |> Document.map Pages
        , toMsg = Shared
        }
        model.shared


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Shared.subscriptions model.shared
            |> Sub.map Shared
        , Pages.subscriptions model.page
            |> Sub.map Pages
        ]



-- URL


fromUrl : Url -> Route
fromUrl =
    Route.fromUrl >> Maybe.withDefault Route.NotFound
