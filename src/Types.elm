module Types exposing
    ( HSL(..)
    , Model
    , Msg(..)
    , ThemeColor
    , colorFromComponents
    , getThemeColorComponent
    , getThemeColors
    , hslToString
    , hsluvToString
    , init
    , rgbToString
    , setComponentValue
    , setThemeColorComponent
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
    , normalizedValue : Float
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


setComponentValue : HSL -> Float -> ThemeColorComponents -> ThemeColorComponents
setComponentValue hsl v cs =
    let
        c =
            getThemeColorComponent hsl cs

        normalized =
            case hsl of
                Hue ->
                    modClamp 360 v

                _ ->
                    clamp 0 100 v

        newC =
            { c
                | input = Round.round 2 normalized
                , valid = True
                , value = v
                , normalizedValue = normalized
            }
    in
    setThemeColorComponent hsl newC cs


modClamp : number -> number -> number
modClamp m f =
    if f < 0 then
        modClamp m (f + m)

    else if f > m then
        modClamp m (f - m)

    else
        f


getThemeColorComponent : HSL -> ThemeColorComponents -> ThemeColorComponent
getThemeColorComponent c cs =
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


colorFromComponents : ThemeColorComponents -> Color
colorFromComponents c =
    HSLuv.hsluv
        { hue = c.h.normalizedValue / 360
        , saturation = c.s.normalizedValue / 100
        , lightness = c.l.normalizedValue / 100
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
