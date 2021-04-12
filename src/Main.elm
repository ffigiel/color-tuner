module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
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
import Parser exposing ((|.), (|=), Parser)
import Round



-- TYPES


type Msg
    = GotRgbInput Int Int String
    | GotHsluvInput Int Int String
    | RemoveColorSetItem Int Int
    | AddColorSetItem Int


type ThemeColor
    = ThemeColorRgb Color
    | ThemeColorHSLuv HSLuv


type alias Model =
    { colorSets : Array ColorSet }


type alias ColorSet =
    { name : String
    , items : Array ColorSetItem
    }


type alias ColorSetItem =
    { color : ThemeColor
    , rgbInput : String
    , rgbValid : Bool
    , hsluvInput : String
    , hsluvValid : Bool
    }


type alias NormalizedColor =
    { rgb : Color
    , hsluv : HSLuv
    }


newColorSetItem : ThemeColor -> ColorSetItem
newColorSetItem c =
    let
        color =
            normalizeColor c
    in
    { color = c
    , rgbInput = rgbToString color.rgb
    , rgbValid = True
    , hsluvInput = hsluvToString color.hsluv
    , hsluvValid = True
    }


normalizeColor : ThemeColor -> NormalizedColor
normalizeColor tc =
    case tc of
        ThemeColorRgb rgb ->
            { rgb = rgb
            , hsluv = rgbToHsluv rgb
            }

        ThemeColorHSLuv hsluv ->
            { rgb = hsluvToRgb hsluv
            , hsluv = hsluv
            }


rgbToHsluv : Color -> HSLuv
rgbToHsluv rgb =
    toRgb rgb |> HSLuv.rgba


hsluvToRgb : HSLuv -> Color
hsluvToRgb hsluv =
    HSLuv.toRgba hsluv
        |> fromRgb


parseRgb : String -> Maybe Color
parseRgb s =
    let
        cleanStr =
            s
                |> String.trim
                |> String.replace "#" ""

        resultR =
            Hex.fromString <| String.slice 0 2 cleanStr

        resultG =
            Hex.fromString <| String.slice 2 4 cleanStr

        resultB =
            Hex.fromString <| String.slice 4 6 cleanStr
    in
    if String.length cleanStr == 6 then
        Result.map3
            (\r g b ->
                rgb255 r g b
            )
            resultR
            resultG
            resultB
            |> Result.toMaybe

    else
        Nothing


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


parseHsluv : String -> Maybe HSLuv
parseHsluv s =
    s
        |> String.toLower
        |> Parser.run hsluvParser
        |> Result.toMaybe


hsluvParser : Parser HSLuv
hsluvParser =
    let
        hsluvFromFloats h s l =
            HSLuv.hsluv360
                { hue = h
                , saturation = s
                , lightness = l
                , alpha = 1
                }
    in
    Parser.succeed hsluvFromFloats
        |. Parser.symbol "hsluv("
        |. Parser.spaces
        |= Parser.float
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.float
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.float
        |. Parser.spaces
        |. Parser.symbol ")"


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
    let
        rgbColors =
            [ ThemeColorRgb <| rgb255 0 0 0
            , ThemeColorRgb <| rgb255 50 0 0
            , ThemeColorRgb <| rgb255 100 0 0
            , ThemeColorRgb <| rgb255 150 0 0
            , ThemeColorRgb <| rgb255 200 0 0
            , ThemeColorRgb <| rgb255 250 0 0
            ]

        hsluvColors =
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

        colorsToItems colors =
            colors
                |> List.map newColorSetItem
                |> Array.fromList
    in
    { colorSets =
        Array.fromList
            [ { name = "rgb"
              , items = colorsToItems rgbColors
              }
            , { name = "hsluv"
              , items = colorsToItems hsluvColors
              }
            ]
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotRgbInput setId itemId value ->
            let
                updateItem item =
                    let
                        newItem =
                            case parseRgb value of
                                Just color ->
                                    { item
                                        | rgbValid = True
                                        , color = ThemeColorRgb color
                                        , hsluvInput = hsluvToString <| rgbToHsluv color
                                    }

                                Nothing ->
                                    { item | rgbValid = False }
                    in
                    { newItem | rgbInput = value }
            in
            { model | colorSets = updateColorSetItem setId itemId updateItem model.colorSets }

        GotHsluvInput setId itemId value ->
            let
                updateItem item =
                    let
                        newItem =
                            case parseHsluv value of
                                Just color ->
                                    { item
                                        | rgbValid = True
                                        , color = ThemeColorHSLuv color
                                        , rgbInput = rgbToString <| hsluvToRgb color
                                    }

                                Nothing ->
                                    { item | hsluvValid = False }
                    in
                    { newItem | hsluvInput = value }
            in
            { model | colorSets = updateColorSetItem setId itemId updateItem model.colorSets }

        RemoveColorSetItem setId itemId ->
            { model | colorSets = removeColorSetItem setId itemId model.colorSets }

        AddColorSetItem setId ->
            let
                newItem =
                    newColorSetItem <| ThemeColorRgb <| rgb 0 0 0
            in
            { model | colorSets = addColorSetItem setId newItem model.colorSets }


updateColorSetItem :
    Int
    -> Int
    -> (ColorSetItem -> ColorSetItem)
    -> Array ColorSet
    -> Array ColorSet
updateColorSetItem setId itemId updateItem colorSets =
    let
        updateColorSet set =
            { set | items = Array.update itemId updateItem set.items }
    in
    Array.update setId updateColorSet colorSets


removeColorSetItem : Int -> Int -> Array ColorSet -> Array ColorSet
removeColorSetItem setId itemId colorSets =
    let
        updateColorSet set =
            { set | items = Array.removeAt itemId set.items }
    in
    Array.update setId updateColorSet colorSets


addColorSetItem : Int -> ColorSetItem -> Array ColorSet -> Array ColorSet
addColorSetItem setId newItem colorSets =
    let
        updateColorSet set =
            { set | items = Array.push newItem set.items }
    in
    Array.update setId updateColorSet colorSets



-- VIEW


rem : number
rem =
    16


errColor : Color
errColor =
    rgb255 220 0 0


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
            |> List.indexedMap colorSetView
        )


colorSetView : Int -> ColorSet -> Element Msg
colorSetView setId colorSet =
    let
        renderedItems =
            colorSet.items
                |> Array.toList
                |> List.indexedMap (colorSetItemView setId)

        addNewButton =
            Input.button
                [ width <| px (rem * 3)
                , height <| px (rem * 3)
                ]
                { onPress = Just <| AddColorSetItem setId
                , label = el [ centerX, centerY ] <| text "Add"
                }
    in
    column
        [ spacingDefault
        , width fill
        , alignTop
        ]
        [ text colorSet.name
        , column []
            (renderedItems ++ [ addNewButton ])
        ]


colorSetItemView : Int -> Int -> ColorSetItem -> Element Msg
colorSetItemView setId itemId item =
    let
        color =
            normalizeColor item.color
    in
    row [ spacingDefault ]
        [ el
            [ Background.color color.rgb
            , width <| px (rem * 3)
            , height <| px (rem * 3)
            ]
            (text "")
        , colorSetItemInput
            { label = "RGB"
            , onChange = GotRgbInput setId itemId
            , text = item.rgbInput
            , valid = item.rgbValid
            }
        , colorSetItemInput
            { label = "HSLuv"
            , onChange = GotHsluvInput setId itemId
            , text = item.hsluvInput
            , valid = item.hsluvValid
            }
        , Input.button
            []
            { onPress = Just <| RemoveColorSetItem setId itemId
            , label = text "Remove"
            }
        ]


colorSetItemInput :
    { label : String
    , onChange : String -> Msg
    , text : String
    , valid : Bool
    }
    -> Element Msg
colorSetItemInput { label, onChange, text, valid } =
    let
        baseAttrs =
            [ Font.family [ Font.monospace ] ]

        attrs =
            if valid then
                baseAttrs

            else
                Border.color errColor :: baseAttrs
    in
    Input.text
        attrs
        { label = Input.labelHidden label
        , onChange = onChange
        , text = text
        , placeholder = Nothing
        }
