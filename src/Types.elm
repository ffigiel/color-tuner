module Types exposing
    ( HSL(..)
    , Model
    , Msg(..)
    , ThemeColor
    , ThemeColorComponent
    , ThemeColorComponents
    , colorFromComponents
    , componentFromValue
    , getThemeColorComponent
    , getThemeColors
    , hslComponents
    , hslToString
    , init
    , rgbToString
    , setComponentValue
    , setThemeColorComponent
    )

import Color exposing (Color)
import Dict exposing (Dict)
import HSLuv
import Hex
import Round



-- Basics


type Msg
    = GotInputText String
    | GotHsluvTextInput String HSL String
    | GotHsluvRangeInput String HSL Float
    | GotAverageRangeInput HSL Float Float
    | ToggleZoom HSL


type alias Model =
    { inputText : String
    , inputErrors : List String
    , themeColorNames : List String
    , themeColorsByName : Dict String ThemeColor
    , zoom : Maybe HSL
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
    in
    { inputText = inputText
    , inputErrors = []
    , themeColorNames = []
    , themeColorsByName = Dict.empty
    , zoom = Nothing
    }


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


hslComponents : List HSL
hslComponents =
    [ Hue, Saturation, Lightness ]


hslToString : HSL -> String
hslToString c =
    case c of
        Hue ->
            "Hue"

        Saturation ->
            "Saturation"

        Lightness ->
            "Lightness"


componentFromValue : HSL -> Float -> ThemeColorComponent
componentFromValue hsl v =
    { input = Round.round 2 v
    , valid = True
    , value = v
    , normalizedValue = normalizeComponentValue hsl v
    }


setComponentValue : Maybe String -> HSL -> Float -> ThemeColorComponents -> ThemeColorComponents
setComponentValue mInput hsl v cs =
    let
        c =
            getThemeColorComponent hsl cs

        normalized =
            normalizeComponentValue hsl v

        newC =
            { c
                | input = Maybe.withDefault (Round.round 2 normalized) mInput
                , valid = True
                , value = v
                , normalizedValue = normalized
            }
    in
    setThemeColorComponent hsl newC cs


normalizeComponentValue : HSL -> Float -> Float
normalizeComponentValue hsl v =
    case hsl of
        Hue ->
            modClamp 360 v

        _ ->
            clamp 0 100 v


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
        |> Color.fromRgba



-- External color utils


rgbToString : Color -> String
rgbToString color =
    let
        components =
            Color.toRgba color

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
