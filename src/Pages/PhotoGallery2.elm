port module Pages.PhotoGallery2 exposing (Params, Model, Msg, page)

import Api.Gallery      exposing
    ( Model
    , ThumbnailSize(..)
    , Status(..)
    , sizeToString
    , activityChanges
    , setFilters
    , photoDecoder
    )
import Api.Photo        exposing (Photo)
import Element          exposing (..)
import Element.Events   as Events
import Element.Input    as Input
import Html             exposing (Html, canvas, label, input)
import Html.Attributes  exposing (checked, id, name, type_)
import Html.Events      exposing (on, onClick)
import Http
import Result           exposing (Result)
import Shared           exposing (urlPrefix)
import Spa.Document     exposing (Document)
import Spa.Page         as Page exposing (Page)
import Spa.Url          exposing (Url)
import UI
import Json.Decode          as Decode  exposing (Decoder, at, float, list)
import Json.Decode.Pipeline            exposing (required)


port storeState2 : String -> Cmd msg


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


type Folder
    = Folder
        { name       : String
        --, photoUrls  : List String
        --, subfolders : List Folder
        , expanded   : Bool
        }


-- INIT


type alias Params =
    ()


type alias Model =
    { status     : Status
    , activity   : String
    , chosenSize : ThumbnailSize
    , hue        : Float
    , ripple     : Float
    , noise      : Float
    , message    : String
    , number     : Int
    }


initialModel : Model
initialModel =
    { status     = Loading
    , activity   = ""
    , chosenSize = Medium
    , hue        = 5
    , ripple     = 5
    , noise      = 5
    , message    = ""
    , number     = 1
{--    , folder     =
        Folder
            { name       = "Loading..."
            , expanded   = True
            --, photoUrls  = []
            --, subfolders = []
            }
--}
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat shared.version
    in
        ( { initialModel | activity = activity }, initialCmd )



-- UPDATE



type Msg
    = ClickedPhoto   String
    | ClickedSize    ThumbnailSize
    | PushData
    | ClickedSurpriseMe
    | GotActivity    String
    | GotPhotos      (Result Http.Error (List Photo))
    | GotRandomPhoto Photo
    | ReloadCanvas
    | SlidHue        Float
    | SlidNoise      Float
    | SlidRipple     Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            let
                newNumber : Int
                newNumber =
                    case Decode.decodeString decodeNumber numberData of
                        Ok value ->
                            value.aNumber

                        Err _ ->
                            0

            in
                ( { model | number = newNumber }, Cmd.none )
            --newFunction model

        PushData ->
            ( model, storeState2 numberData )

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

        GotPhotos (Ok photos) ->
            applyFilters
                { model
                    | status =
                        case photos of
                            first :: rest ->
                                Loaded photos first.url

                            [] ->
                                Loaded [] ""
                }

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        ReloadCanvas ->
            reload model

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }


newData : String
newData =
    "{\"foldersModel\":{\"selectedPhotoUrl\":\"\",\"root\":{\"name\":\"All Photos\",\"expanded\":true,\"photoUrls\":[],\"subfolders\":[{\"name\":\"2016\",\"expanded\":true,\"photoUrls\":[],\"subfolders\":[{\"name\":\"Aloha\",\"expanded\":true,\"photoUrls\":[\"2turtles\",\"beach\",\"wake\"],\"subfolders\":[]},{\"name\":\"Metal\",\"expanded\":true,\"photoUrls\":[\"Joakim\",\"Noora\",\"epica\"],\"subfolders\":[]}]},{\"name\":\"2017\",\"expanded\":true,\"photoUrls\":[],\"subfolders\":[]}]}},\"galleryModel\":{\"activity\":\"Rendering a 500x667 image...\",\"chosenSize\":\"small\",\"hue\":5,\"ripple\":7.66826923076923,\"noise\":5}}"


type alias ThisNumber =
    { aNumber : Int }


numberData : String
numberData =
    """
    {"myNumber": 42}
    """


--newFunction : Model -> ( Model, Cmd Msg )
--newFunction model =
--    ( decodeNumber numberData model, Cmd.none )


decodeNumber : Decoder ThisNumber
decodeNumber =
    Decode.succeed ThisNumber
        |> required "myNumber" Decode.int


{--loadFunction : Decoder Folder -> Folder
loadFunction decoder =
    case (Result decoder) of
        Ok data ->
            data

        Err _ ->
            Folder
                { name       = "Loading..."
                , expanded   = True
                , photoUrls  = []
                , subfolders = []
                }

decodeState : String -> Decoder Folder
decodeState message =
    Decode.succeed Folder
        |> required "root" (decodeFolders message)


decodeFolders : String -> Decoder Folder
decodeFolders message =
    Decode.succeed Folder
        |> required "name" Decode.string
        --|> required "photoUrls" Decode.list Decode.string
        --|> required "subfolders" Decode.list decodeFolders
        |> required "expanded" Decode.bool

--}

save : Model -> Shared.Model -> Shared.Model
save model shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    ( model, Cmd.none)


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "https://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue",    amount = model.hue    / 11 }
                    , { name = "Ripple", amount = model.ripple / 11 }
                    , { name = "Noise",  amount = model.noise  / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored errorMessage ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored errorMessage ->
            status


reload : Model -> (Model, Cmd Msg)
reload newModel =
    applyFilters newModel


viewFilter : (Float -> msg) -> String -> Float -> Element msg
viewFilter toMsg name magnitude =
    row
        UI.filterSlider
        [ el
            UI.filterLabel
            ( text name )
        , el
            ( UI.sliderWrapper
            )
            ( Input.slider
                ( UI.slider
                ++ [ htmlAttribute (onSlide toMsg) ] )
                { onChange = toMsg
                , label = Input.labelHidden "Filter Slider"
                , min = 0
                , max = 11
                , value = magnitude
                , thumb = UI.thumb
                , step = Nothing
                }
            )
        , el
            UI.filterLabel
            ( text (String.fromInt (round magnitude)) )
        ]


onSlide : (Float -> msg) -> Html.Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] float
            |> Decode.map toMsg
            |> on "slide"


subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Photo Gallery"
    , body =
        [ row UI.photoGalleryContent <|
            case model.status of
                Loaded photos selectedUrl ->
                    viewLoaded photos selectedUrl model

                Loading ->
                    []

                Errored errorMessage ->
                    [ text ("Error: " ++ errorMessage) ]
        ]
    }


viewLoaded : List Photo -> String -> Model -> List (Element Msg)
viewLoaded photos selectedUrl model =
    [ column
        [ width fill ]
        [ row -- Activity top right
            UI.activity
            [ text model.activity ]
        , row -- First row (Size, Sliders, Button)
            UI.galleryOptions
            [ column
                UI.thumbnailLabel
                [ text "Thumbnail Size:" ]
            , row
                UI.chosenSize
                (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
            , row
                [ width (px 318) ]
                [ column
                    [ alignRight ]
                    [ viewFilter SlidHue    "Hue"    model.hue
                    , viewFilter SlidRipple "Ripple" model.ripple
                    , viewFilter SlidNoise  "Noise"  model.noise
                    ]
                ]
            , Input.button
                UI.button
                { onPress = Just PushData
                , label   = (text "Push Data!")
                }
            , Input.button
                UI.button
                { onPress = Just ClickedSurpriseMe
                , label   = (text "Get Data!")
                }
            ]
        , row -- Main content (Thumbnails and Canvas)
            UI.contentWrapper
            [ wrappedRow
                UI.thumbnails
                (List.map (viewThumbnail selectedUrl (sizeToString model.chosenSize)) photos)
            , el
                []
                (el
                    UI.image
                    (el
                        UI.canvas
                        (Element.html
                            (canvas
                                [ id "main-canvas" ]
                                []
                            )
                        )
                    )
                )
            ]
        ]
    ]


viewThumbnail : String -> String -> Photo -> Element Msg
viewThumbnail selectedUrl size thumb =
    let
        isSelected =
            (selectedUrl == thumb.url)
    in
    el
        (UI.thumbnailPadding isSelected)
        (el
            (UI.thumbnailBorder isSelected)
            (image
                (
                    (UI.thumbnailSize size)
                    ++ [ Events.onClick (ClickedPhoto thumb.url) ]
                )
                { src = (urlPrefix ++ thumb.url)
                , description = (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
                }
            )
        )


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Element Msg
viewSizeChooser chosenSize size =
    {--Input.radio
        []
        { onChange =
        ,
        }--}
    Element.html (label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ,
            if size == chosenSize then
                checked True
            else
                checked False
        ] []
        , Html.text (sizeToString size)
        ])