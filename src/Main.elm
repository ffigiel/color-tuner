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
    | Noop
    | GotHsluvTextInput Int String
    | GotHsluvRangeInput Int HsluvComponent Float


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
    , hsluvComponents : HsluvComponents
    }


type alias HsluvComponents =
    { hue360 : Float
    , saturation : Float
    , lightness : Float

    -- We don't use alpha, but we keep it for compatibility with HSLuv module
    , alpha : Float
    }


toHsluvComponents : HSLuv -> HsluvComponents
toHsluvComponents hsluv =
    let
        c =
            HSLuv.toHsluv hsluv
    in
    { hue360 = c.hue * 360
    , saturation = c.saturation * 100
    , lightness = c.lightness * 100
    , alpha = c.alpha * 100
    }


fromHsluvComponents : HsluvComponents -> HSLuv
fromHsluvComponents c =
    HSLuv.hsluv
        { hue = c.hue360 / 360
        , saturation = c.saturation / 100
        , lightness = c.lightness / 100
        , alpha = c.alpha / 100
        }


type HsluvComponent
    = Hue
    | Saturation
    | Lightness


rgbToHsluv : Color -> HSLuv
rgbToHsluv rgb =
    toRgb rgb |> HSLuv.rgba


hsluvToRgb : HSLuv -> Color
hsluvToRgb hsluv =
    HSLuv.toRgba hsluv
        |> fromRgb


rgbToString : Color -> String
rgbToString color =
    let
        components =
            toRgb color

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
        initText =
            """--green-50: #ECFDF5;
--green-100: #D1FAE5;
--green-200: #A7F3D0;
--green-300: #6EE7B7;
--green-400: #34D399;
--green-500: #10B981;
--green-600: #059669;
--green-700: #047857;
--green-800: #065F46;
--green-900: #064E3B;"""

        model =
            { inputText = ""
            , themeColors = Array.fromList []
            }
    in
    model
        |> update (GotInputText initText)



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotInputText value ->
            { model
                | inputText = value
                , themeColors = Array.fromList <| parseCssInput value
            }

        GotHsluvTextInput itemId value ->
            let
                updateItem item =
                    case parseHsluv value of
                        Just hsluv ->
                            { item
                                | hsluvInput = value
                                , hsluvValid = True
                                , hsluvComponents = toHsluvComponents hsluv
                                , newColor = hsluvToRgb hsluv
                            }

                        Nothing ->
                            { item
                                | hsluvInput = value
                                , hsluvValid = False
                            }
            in
            { model | themeColors = Array.update itemId updateItem model.themeColors }

        GotHsluvRangeInput itemId component value ->
            let
                updateItem item =
                    let
                        newComponents =
                            setHsluvComponent component value item.hsluvComponents

                        newHsluv =
                            fromHsluvComponents newComponents
                    in
                    { item
                        | newColor = hsluvToRgb newHsluv
                        , hsluvComponents = newComponents
                        , hsluvInput = hsluvToString <| newHsluv
                    }
            in
            { model | themeColors = Array.update itemId updateItem model.themeColors }

        Noop ->
            model


hsluvComponentToString : HsluvComponent -> String
hsluvComponentToString c =
    case c of
        Hue ->
            "hue"

        Saturation ->
            "saturation"

        Lightness ->
            "lightness"


getHsluvComponent : HsluvComponent -> HsluvComponents -> Float
getHsluvComponent c cs =
    case c of
        Hue ->
            cs.hue360

        Saturation ->
            cs.saturation

        Lightness ->
            cs.lightness


setHsluvComponent : HsluvComponent -> Float -> HsluvComponents -> HsluvComponents
setHsluvComponent c v cs =
    case c of
        Hue ->
            { cs | hue360 = v }

        Saturation ->
            { cs | saturation = v }

        Lightness ->
            { cs | lightness = v }


parseCssInput : String -> List ThemeColor
parseCssInput value =
    let
        newItem ( name, color ) =
            let
                hsluv =
                    rgbToHsluv color
            in
            Just
                { name = name
                , originalColor = color
                , newColor = color
                , hsluvInput = hsluvToString hsluv
                , hsluvValid = True
                , hsluvComponents = toHsluvComponents hsluv
                }
    in
    value
        |> Parser.run cssInputParser
        |> Result.withDefault []
        |> List.filterMap newItem


type alias CssColorStmt =
    ( String, Color )


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


cssColorParser : Parser Color
cssColorParser =
    Parser.oneOf
        [ Parser.backtrackable hex6ColorParser
        , hex3ColorParser
        ]


hex6ColorParser : Parser Color
hex6ColorParser =
    Parser.succeed rgb255
        |. Parser.symbol "#"
        |= hex255Parser
        |= hex255Parser
        |= hex255Parser


hex3ColorParser : Parser Color
hex3ColorParser =
    Parser.succeed rgb255
        |. Parser.symbol "#"
        |= hex255ShorthandParser
        |= hex255ShorthandParser
        |= hex255ShorthandParser


hex255Parser : Parser Int
hex255Parser =
    Parser.mapChompedString
        (\s () ->
            s
                |> String.toLower
                |> Hex.fromString
                |> Result.withDefault 0
        )
        (Parser.succeed ()
            |. Parser.chompIf Char.isHexDigit
            |. Parser.chompIf Char.isHexDigit
        )


hex255ShorthandParser : Parser Int
hex255ShorthandParser =
    Parser.mapChompedString
        (\s () ->
            s
                ++ s
                |> String.toLower
                |> Hex.fromString
                |> Result.withDefault 0
        )
        (Parser.succeed ()
            |. Parser.chompIf Char.isHexDigit
        )



-- VIEW


rem : number
rem =
    16


errColor : Color
errColor =
    rgb255 220 0 0


gray : Color
gray =
    rgb255 220 220 220


linkColor : Color
linkColor =
    rgb255 0 0 220


spacingSmall : Attribute Msg
spacingSmall =
    spacing <| rem


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
        , fontMonospace
        ]
    <|
        column
            [ spacingDefault
            , paddingDefault
            , width (fill |> maximum (rem * 100))
            , height fill
            , centerX
            ]
            [ column
                [ width fill
                , height fill
                ]
                [ appView model ]
            , footerView
            ]


appView : Model -> Element Msg
appView model =
    column [ spacingDefault, width fill ]
        [ row [ spacingDefault, width fill ]
            [ inputView model.inputText
            , outputView model.themeColors
            ]
        , themeColorsView model.themeColors
        ]


themeColorsView : Array ThemeColor -> Element Msg
themeColorsView themeColors =
    if Array.isEmpty themeColors then
        el
            [ Font.color errColor ]
            (text "No colors found.")

    else
        column [ spacingSmall, width fill ]
            [ themeColorsHeaderView
            , column [ width fill ]
                (themeColors
                    |> Array.toList
                    |> List.indexedMap themeColorView
                )
            ]


themeColorsHeaderView : Element Msg
themeColorsHeaderView =
    let
        nameColWidth =
            7 * rem

        previewColWidth =
            (3 + 3 + 1 + 3 + 3) * rem
    in
    row [ spacingDefault, width fill, Font.center ]
        [ el [ width <| px nameColWidth ] (text "Color")
        , el [ width fill ] (text "HSLuv")
        , el [ width fill ] (text "Hue")
        , el [ width fill ] (text "Saturation")
        , el [ width fill ] (text "Lightness")
        , el [ width <| px previewColWidth ] (text "Preview")
        ]


themeColorView : Int -> ThemeColor -> Element Msg
themeColorView itemId item =
    let
        rangeInputs =
            [ Hue, Saturation, Lightness ]
                |> List.map
                    (\c ->
                        hsluvRangeInput
                            { component = c
                            , onChange = GotHsluvRangeInput itemId c
                            , value = getHsluvComponent c item.hsluvComponents
                            }
                    )

        children =
            [ el [ width <| px (7 * rem) ]
                (text item.name)
            , hsluvInput
                { label = item.name
                , onChange = GotHsluvTextInput itemId
                , value = item.hsluvInput
                , valid = item.hsluvValid
                }
            ]
                ++ rangeInputs
                ++ [ row [ spacingSmall ]
                        [ row []
                            [ colorSwatch item.originalColor
                            , colorSwatch item.newColor
                            ]
                        , row []
                            [ textSwatch item.originalColor
                            , textSwatch item.newColor
                            ]
                        ]
                   ]
    in
    row [ spacingDefault, width fill ] children


colorSwatch : Color -> Element Msg
colorSwatch color =
    el
        [ Background.color color
        , width <| px (rem * 3)
        , height <| px (rem * 3)
        ]
        (text "")


textSwatch : Color -> Element Msg
textSwatch color =
    el
        [ Font.color color
        , width <| px (rem * 3)
        , height <| px (rem * 3)
        , centerX
        , centerY
        ]
        (el [ centerX, centerY ] (text "Aa"))


hsluvInput :
    { label : String
    , onChange : String -> Msg
    , value : String
    , valid : Bool
    }
    -> Element Msg
hsluvInput { label, onChange, value, valid } =
    let
        attrs =
            if valid then
                []

            else
                [ Border.color errColor ]
    in
    Input.text
        attrs
        { label = Input.labelHidden label
        , onChange = onChange
        , text = value
        , placeholder = Nothing
        }


hsluvRangeInput :
    { component : HsluvComponent, onChange : Float -> Msg, value : Float }
    -> Element Msg
hsluvRangeInput { component, onChange, value } =
    let
        sliderMax =
            case component of
                Hue ->
                    360

                _ ->
                    100
    in
    Input.slider
        [ height <| px rem
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color gray
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = onChange
        , label = Input.labelHidden <| hsluvComponentToString component
        , min = 0
        , max = sliderMax
        , step = Just 0.01
        , value = value
        , thumb =
            Input.defaultThumb
        }


inputView : String -> Element Msg
inputView value =
    Input.multiline [ width fill ]
        { onChange = GotInputText
        , text = value
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Input")
        , spellcheck = False
        }


outputView : Array ThemeColor -> Element Msg
outputView colors =
    let
        outputText =
            colors
                |> Array.toList
                |> List.map toCssValue
                |> String.join "\n"

        toCssValue item =
            item.name
                ++ ": "
                ++ rgbToString item.newColor
                ++ ";"
    in
    Input.multiline [ width fill, height fill ]
        { onChange = always Noop
        , text = outputText
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Output")
        , spellcheck = False
        }


footerView : Element Msg
footerView =
    row
        [ spacingDefault
        , width fill
        ]
        [ newTabLink
            [ Font.color linkColor, Font.underline ]
            { url = "https://github.com/megapctr/color-tuner"
            , label = text "Source"
            }
        ]
