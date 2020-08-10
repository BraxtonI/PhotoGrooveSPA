module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    , urlPrefix
    , Photo
    )

import Browser.Navigation   exposing (Key)
import Element              exposing (..)
import Element.Region       as Region
import Dict                 exposing (Dict)
import Spa.Document         exposing (Document)
import Spa.Generated.Route  as Route
import Url                  exposing (Url)
import UI



-- INIT


type alias Flags =
    ()


type alias Model =
    { url                     : Url
    , key                     : Key
    , foldersSelectedFilename : Maybe String
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        selectedFilename : Maybe String
        selectedFilename =
            Nothing

    in
    ( Model url key selectedFilename
    , Cmd.none
    )


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type alias Photo =
    { title       : String
    , url         : String
    , size        : Int
    , relatedUrls : List String
    }



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    { page : Document msg, toMsg : Msg -> msg }
    -> Model
    -> Document msg
view { page, toMsg } model =
    let
        navLink : Route.Route -> String -> Element msg
        navLink url label =
            link
                (UI.nav (isActive url page.title))
                { url = Route.toString url
                , label = text label
                }
    in
    { title = page.title
    , body =
        [ column
            [ height fill, width fill ]
            [ row
                [ Region.navigation ]
                [ el
                    UI.h1
                    (text "Photo Groove")
                , navLink Route.PhotoFolders "Photo Folders"
                , navLink Route.PhotoGallery "Photo Gallery"
                ]
            , column
                []
                page.body
            , el
                UI.footer
                (text "One is never alone with a rubber duck. -Douglas Adams")
            ]
        ]
    }


isActive : Route.Route -> String -> Bool
isActive link page =
    case ( link, page ) of

        ( Route.PhotoFolders , "Photo Folders" ) -> True

        ( Route.PhotoFolders , _               ) -> False

        ( Route.PhotoGallery , "Photo Gallery" ) -> True

        ( Route.PhotoGallery , _               ) -> False

        ( Route.Top          , _               ) -> False

        ( _                  , _               ) -> False