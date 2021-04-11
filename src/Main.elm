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
import Hex
import Html exposing (Html)



-- TYPES


type Msg
    = None


type ThemeColor
    = ThemeColorRgb Color
    | ThemeColorHSLuv HSLuv


type alias Model =
    { colors : List ThemeColor }


type alias NormalizedColor =
    { rgb : Color
    , hsluv : HSLuv
    }


normalizeColor : ThemeColor -> NormalizedColor
normalizeColor tc =
    case tc of
        ThemeColorRgb rgb ->
            { rgb = rgb
            , hsluv = toRgb rgb |> HSLuv.rgba
            }

        ThemeColorHSLuv hsluv ->
            { rgb = HSLuv.toRgba hsluv |> fromRgb
            , hsluv = hsluv
            }


rgbToString : Color -> String
rgbToString color =
    let
        components =
            toRgb color

        componentToString : Float -> String
        componentToString =
            (*) 255
                >> floor
                >> Hex.toString
                >> String.padLeft 2 '0'
    in
    [ components.red, components.green, components.blue ]
        |> List.map componentToString
        |> String.join ""
        |> (++) "#"



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
        [ ThemeColorRgb <| rgb255 0 0 0
        , ThemeColorRgb <| rgb255 100 200 150
        , ThemeColorRgb <| rgb255 255 255 255
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
        [ text <| rgbToString color.rgb
        , text <| Debug.toString color.hsluv
        ]
