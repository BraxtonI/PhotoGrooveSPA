module UI exposing (..)

import Element            exposing (..)
import Element.Background as Background
import Element.Border     as Border
import Element.Font       as Font
import Element.Input      as Input
import Element.Region     as Region


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top    = 0
    , right  = 0
    , bottom = 0
    , left   = 0
    }


fontColor : Color
fontColor =
    rgb255 250 250 250


backgroundColor : Color
backgroundColor =
    rgb255 44  44  44


white : Color
white =
    rgb255 255 255 255


blue : Color
blue =
    rgb255 96 181 204


grey : Color
grey =
    rgb255 85 85 85


body : List (Attribute msg)
body =
    [ Font.family
        [ Font.typeface "Verdana" ]
    , Background.color  backgroundColor
    , Font.color        fontColor
    , width             fill
    , height            fill
    ]


h1 : List (Attribute msg)
h1 =
    [ Font.size   32
    , Font.bold
    , Font.color  blue
    , paddingEach { edges | top = 10 , left = 5, right = 20 }
    ]


h2 : List (Attribute msg)
h2 =
    [ Font.size   24
    , paddingEach { edges | top = 13, bottom = 13 }
    , Font.bold
    ]


h3 : List (Attribute msg)
h3 =
    [ paddingEach { edges | top = 60, bottom = 20 }
    , Font.color  blue
    , Font.bold
    ]



nav : Bool -> List (Attribute msg)
nav active =
    let
        maybeActive : List (Attribute msg)
        maybeActive =
            if active then
                [ Font.underline ]
            else
                []

    in
    List.append
        maybeActive
        [ paddingEach { edges | top = 10, left = 15, right = 15 }
        , Font.color  white
        , Font.bold
        ]


hoverUnderline : List (Attribute msg)
hoverUnderline =
    [ Border.widthEach { edges | bottom = 2 }
    , Border.color     backgroundColor
    , mouseOver        [ Border.color white ]
    ]



photoFoldersContent : List (Attribute msg)
photoFoldersContent =
    [ padding 40
    , centerX
    ]


folderLabel : List (Attribute msg)
folderLabel =
    [ paddingEach      { edges | top = 6, left = 8, right = 8, bottom = 6 }
    , Background.color grey
    , mouseOver        [ Background.color blue ]
    , pointer
    ]


cascade : List (Attribute msg)
cascade =
    [ paddingEach { edges | left = 12, top = 8 }
    ]



photo : List (Attribute msg)
photo =
    [ paddingEach { edges | top = 6, left = 8, right = 8}
    , pointer
    , Font.color  white
    , mouseOver   [ Background.color blue ]
    ]


selectedPhoto : List (Attribute msg)
selectedPhoto =
    [ alignLeft
    , height
        (fill
            |> minimum 400
        )
    , width (px 600)
    ]


folders : List (Attribute msg)
folders =
    [ alignLeft
    , alignTop
    , height
        (fill
            |> minimum 400
        )
    , width (px 360)
    ]


image : List (Attribute msg)
image =
    [ Border.width 1
    , Border.solid
    , Border.color white
    ]


relatedPhoto : List (Attribute msg)
relatedPhoto =
    List.append image
        [ alignLeft
        , mouseOver [ Border.color blue ]
        , pointer
        ]




activity : List (Attribute msg)
activity =
    [ alignRight
    ]


photoGalleryContent : List (Attribute msg)
photoGalleryContent =
    [ paddingEach { edges | top = 40 }
    , width       (px 960)
    , centerX
    ]


galleryOptions : List (Attribute msg)
galleryOptions =
    [ centerX
    ]


thumbnailLabel : List (Attribute msg)
thumbnailLabel =
    [ paddingEach { edges | top = 20, bottom = 20 }
    , Font.color  blue
    , Font.bold
    , centerY
    ]


button : List (Attribute msg)
button =
    [ Background.color blue
    , alignRight
    , Border.widthEach edges
    , Font.color       backgroundColor
    , Font.size        24
    , pointer
    , padding          10
    , mouseOver        [ Background.color white ]
    ]


chosenSize : List (Attribute msg)
chosenSize =
    [ alignLeft
    , paddingEach { edges | left = 20 }
    ]


filterLabel : List (Attribute msg)
filterLabel =
    [ width       (px 70)
    , paddingEach { edges | top = 5 }
    ]


filterSlider : List (Attribute msg)
filterSlider =
    [
    ]


slider : List (Attribute msg)
slider =
    [ width            (px 120)
    , height           (px 2)
    , Background.color white
    ]


sliderWrapper : List (Attribute msg)
sliderWrapper =
    [ paddingEach { edges | top = 7, right = 7, left = 7 }
    ]


thumb : Input.Thumb
thumb =
    Input.thumb
        [ Element.width     (px 16)
        , Element.height    (px 16)
        , Border.rounded    8
        , Border.width      1
        , Border.color      (rgb 0.6 0.6 0.6)
        , Background.color  (rgb 0.6 0.6 0.6)
        , focused
            [ Background.color blue ]
        ]


thumbnails : List (Attribute msg)
thumbnails =
    [ width (px 440)
    , alignLeft
    , alignTop
    ]


thumbnailSize : String -> List (Attribute msg)
thumbnailSize size =
    let
        maxSize : Int
        maxSize =
            case size of
                "small" ->
                    50

                "med" ->
                    100

                "large" ->
                    200

                _ ->
                    100

    in
    [ width (px maxSize)
    ]


borderSize : Bool -> Int
borderSize selected =
    if selected then
        6
    else
        1


thumbnailBorder : Bool -> List (Attribute msg)
thumbnailBorder selected =
    let
        borderColor =
            if selected then
                blue
            else
                white

    in
    [ Border.color borderColor
    , Border.width (borderSize selected)
    , Border.solid
    ]


thumbnailPadding : Bool -> List (Attribute msg)
thumbnailPadding selected =
    [ padding (7 - (borderSize selected)) ]


canvas : List (Attribute msg)
canvas =
    [ width (px 500)
    , alignRight
    ]


contentWrapper : List (Attribute msg)
contentWrapper =
    [ paddingEach { edges | top = 10 }
    ]



-- Footer


footer : List (Attribute msg)
footer =
    [ Font.color <| rgb255 187 187 187
    , paddingEach { top = 60, right = 20, bottom = 20, left = 20 }
    , Region.footer
    ]