module Pages.PhotoGallery exposing (Params, Model, Msg, page)

import Api.Gallery      exposing
    ( Model
    , initialModel
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
import Json.Decode      exposing (Decoder, at, float, list)
import Random
import Shared           exposing (urlPrefix)
import Spa.Document     exposing (Document)
import Spa.Page         as Page exposing (Page)
import Spa.Url          exposing (Url)
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
    Api.Gallery.Model


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared { params } =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat shared.version
    in
    if shared.galleryModel == initialModel then
        ( { initialModel | activity = activity }, initialCmd )
    else
        load shared initialModel



-- UPDATE



type Msg
    = ClickedPhoto   String
    | ClickedSize    ThumbnailSize
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
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

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


save : Model -> Shared.Model -> Shared.Model
save model shared =
    Tuple.first (Shared.update (Shared.UpdateGallery model) shared)


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load shared model =
    update ReloadCanvas shared.galleryModel


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
            |> Json.Decode.map toMsg
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
                { onPress = Just ClickedSurpriseMe
                , label   = (text "Surprise Me!")
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