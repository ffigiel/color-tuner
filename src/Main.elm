module Main exposing (main)

import Array exposing (Array)
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
import Round



-- TYPES


type Msg
    = None


type ThemeColor
    = ThemeColorRgb Color
    | ThemeColorHSLuv HSLuv


type alias Model =
    { colorSets : Array ColorSet }


type alias ColorSet =
    { name : String
    , colors : Array ThemeColor
    }


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
    { colorSets =
        Array.fromList
            [ { name = "rgb"
              , colors =
                    Array.fromList
                        [ ThemeColorRgb <| rgb255 0 0 0
                        , ThemeColorRgb <| rgb255 50 0 0
                        , ThemeColorRgb <| rgb255 100 0 0
                        , ThemeColorRgb <| rgb255 150 0 0
                        , ThemeColorRgb <| rgb255 200 0 0
                        , ThemeColorRgb <| rgb255 250 0 0
                        ]
              }
            , { name = "hsluv"
              , colors =
                    List.range 0 5
                        |> List.map
                            (\n ->
                                { hue = 12.18
                                , saturation = 100
                                , lightness = 10.442 * toFloat n
                                , alpha = 1
                                }
                                    |> HSLuv.hsluv360
                                    |> ThemeColorHSLuv
                            )
                        |> Array.fromList
              }
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
    row [ spacingDefault ]
        (model.colorSets
            |> Array.toList
            |> List.map colorSetView
        )


colorSetView : ColorSet -> Element Msg
colorSetView colorSet =
    column [ spacingDefault, width fill ]
        [ text colorSet.name
        , column []
            (colorSet.colors
                |> Array.toList
                |> List.map colorView
            )
        ]


colorView : ThemeColor -> Element Msg
colorView tc =
    let
        color =
            normalizeColor tc
    in
    row [ spacingDefault ]
        [ el
            [ Background.color color.rgb
            , width <| px (rem * 3)
            , height <| px (rem * 3)
            ]
            (text "")
        , Input.text
            []
            { label = Input.labelHidden "RGB"
            , onChange = always None
            , text = rgbToString color.rgb
            , placeholder = Nothing
            }
        , Input.text
            []
            { label = Input.labelHidden "HSLuv"
            , onChange = always None
            , text = hsluvToString color.hsluv
            , placeholder = Nothing
            }
        ]
