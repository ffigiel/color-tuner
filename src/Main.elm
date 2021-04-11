module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import HSLuv exposing (HSLuv)
import Html exposing (Html)



-- TYPES


type ThemeColor
    = ThemeColorRGB Color
    | ThemeColorHSLuv HSLuv


type alias NormalizedColor =
    { rgb : Color
    , hsluv : HSLuv
    }


normalizeColor : ThemeColor -> NormalizedColor
normalizeColor tc =
    case tc of
        ThemeColorRGB color ->
            { rgb = color
            , hsluv = toRgb color |> HSLuv.rgba
            }

        ThemeColorHSLuv hsluv ->
            { rgb = HSLuv.toRgba hsluv |> fromRgb
            , hsluv = hsluv
            }


type alias Model =
    { colors : List ThemeColor }


type Msg
    = None



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- INIT


init : Model
init =
    { colors =
        [ ThemeColorRGB <| rgb255 100 200 150
        ]
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        _ ->
            model



-- VIEW


rem : number
rem =
    16


spacingDefault : Attribute Msg
spacingDefault =
    spacing <| rem * 2


paddingDefault : Attribute Msg
paddingDefault =
    padding <| rem * 2


view : Model -> Html Msg
view model =
    layout
        [ Font.size rem
        ]
    <|
        column
            [ spacingDefault
            , paddingDefault
            , width (fill |> maximum (rem * 80))
            , height fill
            , centerX
            ]
            [ appView model
            ]


appView : Model -> Element Msg
appView model =
    column [ spacingDefault ]
        (List.map themeColorView model.colors)


themeColorView : ThemeColor -> Element Msg
themeColorView tc =
    let
        color =
            normalizeColor tc
    in
    row [ spacingDefault ]
        [ text <| Debug.toString color.rgb
        , text <| Debug.toString color.hsluv
        ]
