module Types exposing
    ( HSL(..)
    , HsluvComponents
    , Model
    , Msg(..)
    , ThemeColor
    , colorFromComponents
    , fromHsluvComponents
    , getThemeColorComponents
    , getThemeColors
    , hslToString
    , hsluvToString
    , init
    , rgbToString
    , setThemeColorComponent
    , toHsluvComponents
    )

import Dict exposing (Dict)
import Element exposing (Color)
import HSLuv exposing (HSLuv)
import Hex
import Round



-- Basics


type Msg
    = Noop
    | GotInputText String
    | GotHsluvTextInput String HSL String
    | GotHsluvRangeInput String HSL Float
    | GotAverageRangeInput HSL Float Float


type alias Model =
    { inputText : String
    , inputErrors : List String
    , themeColorNames : List String
    , themeColorsByName : Dict String ThemeColor
    }


init : Model
init =
    let
        inputText =
            """--green-50: rgb(236, 253, 245);
--green-100: hsl(149.3, 80.4%, 90%);
--green-200: #A7F3D0;
--green-300: #6EE7B7;
--green-400: #3D9;
--green-500: #10B981;
--green-600: #059669;
--green-700: #047857;
--green-800: #065F46;
--green-900: #064E3B;"""

        model =
            { inputText = inputText
            , inputErrors = []
            , themeColorNames = []
            , themeColorsByName = Dict.empty
            }
    in
    model


getThemeColors : Model -> List ThemeColor
getThemeColors model =
    model.themeColorNames
        |> List.filterMap (\n -> Dict.get n model.themeColorsByName)


type alias ThemeColor =
    { name : String
    , originalColor : Color
    , newColor : Color
    , components : ThemeColorComponents
    }


type alias ThemeColorComponents =
    { h : ThemeColorComponent
    , s : ThemeColorComponent
    , l : ThemeColorComponent
    }


type alias ThemeColorComponent =
    { input : String
    , valid : Bool
    , value : Float
    }



-- HSL


type HSL
    = Hue
    | Saturation
    | Lightness


hslToString : HSL -> String
hslToString c =
    case c of
        Hue ->
            "hue"

        Saturation ->
            "saturation"

        Lightness ->
            "lightness"



-- HsluvComponents


type alias HsluvComponents =
    { hue360 : Float
    , saturation : Float
    , lightness : Float
    }


getThemeColorComponents : HSL -> ThemeColorComponents -> ThemeColorComponent
getThemeColorComponents c cs =
    case c of
        Hue ->
            cs.h

        Saturation ->
            cs.s

        Lightness ->
            cs.l


setThemeColorComponent : HSL -> ThemeColorComponent -> ThemeColorComponents -> ThemeColorComponents
setThemeColorComponent hsl c cs =
    case hsl of
        Hue ->
            { cs | h = c }

        Saturation ->
            { cs | s = c }

        Lightness ->
            { cs | l = c }


toHsluvComponents : HSLuv -> HsluvComponents
toHsluvComponents hsluv =
    let
        c =
            HSLuv.toHsluv hsluv
    in
    { hue360 = c.hue * 360
    , saturation = c.saturation * 100
    , lightness = c.lightness * 100
    }


fromHsluvComponents : HsluvComponents -> HSLuv
fromHsluvComponents c =
    HSLuv.hsluv
        { hue = c.hue360 / 360
        , saturation = c.saturation / 100
        , lightness = c.lightness / 100
        , alpha = 1
        }


colorFromComponents : ThemeColorComponents -> Color
colorFromComponents c =
    HSLuv.hsluv
        { hue = c.h.value / 360
        , saturation = c.s.value / 100
        , lightness = c.l.value / 100
        , alpha = 1
        }
        |> HSLuv.toRgba
        |> Element.fromRgb



-- External color utils


rgbToString : Color -> String
rgbToString color =
    let
        components =
            Element.toRgb color

        componentToString : Float -> String
        componentToString =
            max 0
                >> (*) 255
                >> floor
                >> Hex.toString
                >> String.padLeft 2 '0'
    in
    [ components.red, components.green, components.blue ]
        |> List.map componentToString
        |> String.join ""
        |> (++) "#"


hsluvToString : HSLuv -> String
hsluvToString color =
    let
        components =
            HSLuv.toHsluv color

        stringComponents =
            [ components.hue * 360
            , components.saturation * 100
            , components.lightness * 100
            ]
                |> List.map smartRound

        smartRound : Float -> String
        smartRound n =
            if n == toFloat (floor n) then
                Round.round 0 n

            else
                Round.round 2 n
    in
    "hsluv("
        ++ String.join ", " stringComponents
        ++ ")"
