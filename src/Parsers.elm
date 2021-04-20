module Parsers exposing (deadEndToString, parseCssInput)

import Color
import Element exposing (Color)
import HSLuv
import Hex
import Parser exposing ((|.), (|=), Parser)
import Set
import Types exposing (HSL(..), ThemeColor, componentFromValue)


parseCssInput : String -> Result (List Parser.DeadEnd) (List ThemeColor)
parseCssInput value =
    let
        newItem : ( String, Color ) -> ThemeColor
        newItem ( name, color ) =
            let
                c =
                    color
                        |> Element.toRgb
                        |> HSLuv.rgba
                        |> HSLuv.toHsluv
            in
            { name = name
            , originalColor = color
            , newColor = color
            , components =
                { h = componentFromValue Hue (c.hue * 360)
                , s = componentFromValue Saturation (c.saturation * 100)
                , l = componentFromValue Lightness (c.lightness * 100)
                }
            }
    in
    value
        |> Parser.run cssInputParser
        |> Result.map (List.map newItem)


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
        [ rgbColorParser
        , hslColorParser
        , hexColorParser
        ]


rgbColorParser : Parser Color
rgbColorParser =
    Parser.succeed Element.rgb255
        |. Parser.symbol "rgb("
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |. Parser.symbol ")"


hslColorParser : Parser Color
hslColorParser =
    let
        hslToColor h s l =
            Color.hsl (h / 360) (s / 100) (l / 100)
                |> Color.toRgba
                |> Element.fromRgb
    in
    Parser.succeed hslToColor
        |. Parser.symbol "hsl("
        |. Parser.spaces
        |= Parser.float
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.float
        |. Parser.symbol "%"
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.float
        |. Parser.symbol "%"
        |. Parser.spaces
        |. Parser.symbol ")"


hexColorParser : Parser Color
hexColorParser =
    Parser.oneOf
        [ Parser.backtrackable hex6ColorParser
        , hex3ColorParser
        ]


hex6ColorParser : Parser Color
hex6ColorParser =
    Parser.succeed Element.rgb255
        |. Parser.symbol "#"
        |= hex255Parser
        |= hex255Parser
        |= hex255Parser


hex3ColorParser : Parser Color
hex3ColorParser =
    Parser.succeed Element.rgb255
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


deadEndToString : Parser.DeadEnd -> String
deadEndToString de =
    let
        problemToString p =
            case p of
                Parser.Expecting s ->
                    "Expected '" ++ s ++ "'"

                Parser.ExpectingSymbol s ->
                    "Expected '" ++ s ++ "'"

                Parser.ExpectingInt ->
                    "Expected an integer"

                Parser.ExpectingFloat ->
                    "Expected a number"

                Parser.ExpectingHex ->
                    "Expected a hex digit"

                _ ->
                    -- Debug.toString p
                    "Invalid input"
    in
    "Line "
        ++ String.fromInt de.row
        ++ ", char "
        ++ String.fromInt de.col
        ++ ": "
        ++ problemToString de.problem
