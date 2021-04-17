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
import Set



-- TYPES


type Msg
    = GotInputText String
    | GotHsluvInput Int String


type alias Model =
    { inputText : String
    , themeColors : Array ThemeColor
    }


type alias ThemeColor =
    { name : String
    , originalColor : Color
    , newColor : Color
    , hsluvInput : String
    , hsluvValid : Bool
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
            if String.length s == 3 then
                s
                    |> String.foldl (\c acc -> acc ++ [ c, c ]) []
                    |> String.fromList

            else
                s

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
            [ rgb255 0 0 0
            , rgb255 50 0 0
            , rgb255 100 0 0
            , rgb255 150 0 0
            , rgb255 200 0 0
            , rgb255 250 0 0
            ]

        toThemeColor i rgbColor =
            { name = String.repeat (i + 1) "a"
            , originalColor = rgbColor
            , newColor = rgbColor
            , hsluvInput = hsluvToString <| rgbToHsluv rgbColor
            , hsluvValid = True
            }

        themeColors =
            rgbColors
                |> List.indexedMap toThemeColor
                |> Array.fromList
    in
    { inputText = ""
    , themeColors = themeColors
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotInputText value ->
            { model
                | inputText = value
                , themeColors = Array.fromList <| parseCssInput value
            }

        GotHsluvInput itemId value ->
            let
                updateItem item =
                    case parseHsluv value of
                        Just color ->
                            { item
                                | hsluvInput = value
                                , hsluvValid = True
                                , newColor = hsluvToRgb color
                            }

                        Nothing ->
                            { item
                                | hsluvInput = value
                                , hsluvValid = False
                            }
            in
            { model | themeColors = Array.update itemId updateItem model.themeColors }


parseCssInput : String -> List ThemeColor
parseCssInput value =
    let
        newItem ( name, mColor ) =
            case mColor of
                Just color ->
                    Just
                        { name = name
                        , originalColor = color
                        , newColor = color
                        , hsluvInput = hsluvToString <| rgbToHsluv color
                        , hsluvValid = True
                        }

                Nothing ->
                    Nothing
    in
    value
        |> Parser.run cssInputParser
        |> Result.toMaybe
        |> Maybe.withDefault []
        |> List.filterMap newItem


type alias CssColorStmt =
    ( String, Maybe Color )


cssInputParser : Parser (List CssColorStmt)
cssInputParser =
    Parser.loop [] cssColorStmtParser


cssColorStmtParser :
    List CssColorStmt
    -> Parser (Parser.Step (List CssColorStmt) (List CssColorStmt))
cssColorStmtParser revStmts =
    Parser.oneOf
        [ Parser.succeed (\k v -> Parser.Loop <| ( k, v ) :: revStmts)
            -- property name
            |. Parser.spaces
            |= cssNameParser
            |. Parser.spaces
            |. Parser.symbol ":"
            -- value
            |. Parser.spaces
            |= cssColorParser
            |. Parser.spaces
            |. Parser.symbol ";"
            -- drop comments
            |. Parser.chompUntilEndOr "\n"
            |. Parser.spaces
        , Parser.succeed (Parser.Done <| List.reverse revStmts)
        ]


cssNameParser : Parser String
cssNameParser =
    Parser.variable
        { start = always True
        , inner = \c -> Char.isAlphaNum c || c == '-'
        , reserved = Set.empty
        }


cssColorParser : Parser (Maybe Color)
cssColorParser =
    Parser.oneOf
        [ Parser.succeed parseRgb
            |. Parser.chompIf ((==) '#')
            |= Parser.getChompedString (Parser.chompWhile Char.isHexDigit)
        ]



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


fontMonospace : Attribute Msg
fontMonospace =
    Font.family [ Font.monospace ]


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
            , width (fill |> maximum (rem * 100))
            , height fill
            , centerX
            ]
            [ appView model
            ]


appView : Model -> Element Msg
appView model =
    column [ spacingDefault, width fill ]
        [ column [ width fill ]
            (model.themeColors
                |> Array.toList
                |> List.indexedMap themeColorView
            )
        , row [ spacingDefault, width fill ]
            [ inputView model.inputText
            , outputView model.themeColors
            ]
        ]


themeColorView : Int -> ThemeColor -> Element Msg
themeColorView itemId item =
    row [ spacingDefault, width fill ]
        [ hsluvInput
            { label = item.name
            , onChange = GotHsluvInput itemId
            , value = item.hsluvInput
            , valid = item.hsluvValid
            }
        , row []
            [ colorSwatch item.originalColor
            , colorSwatch item.newColor
            ]
        ]


colorSwatch : Color -> Element Msg
colorSwatch color =
    el
        [ Background.color color
        , width <| px (rem * 3)
        , height <| px (rem * 3)
        ]
        (text "")


hsluvInput :
    { label : String
    , onChange : String -> Msg
    , value : String
    , valid : Bool
    }
    -> Element Msg
hsluvInput { label, onChange, value, valid } =
    let
        baseAttrs =
            [ fontMonospace ]

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
        , text = value
        , placeholder = Nothing
        }


inputView : String -> Element Msg
inputView value =
    Input.multiline [ width fill, fontMonospace ]
        { onChange = GotInputText
        , text = value
        , placeholder = Nothing
        , label = Input.labelHidden "css"
        , spellcheck = False
        }


outputView : Array ThemeColor -> Element Msg
outputView colors =
    let
        outputText =
            colors
                |> Array.toList
                |> List.map cssValue
                |> String.join "\n"

        cssValue item =
            item.name
                ++ ": "
                ++ rgbToString item.newColor
                ++ ";"
    in
    el [ width fill, fontMonospace ]
        (text outputText)
