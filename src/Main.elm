module Main exposing (main)

import Browser
import Dict
import Element exposing (Color)
import HSLuv exposing (HSLuv)
import Parsers
import Types exposing (..)
import View exposing (view)


hsluvToRgb : HSLuv -> Color
hsluvToRgb hsluv =
    HSLuv.toRgba hsluv
        |> Element.fromRgb



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init |> (\m -> update (GotInputText m.inputText) m)
        , view = view
        , update = update
        }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        GotInputText value ->
            case Parsers.parseCssInput value of
                Ok themeColors ->
                    { model
                        | inputText = value
                        , inputErrors = []
                        , themeColorNames = List.map .name themeColors
                        , themeColorsByName =
                            List.map (\c -> ( c.name, c )) themeColors
                                |> Dict.fromList
                    }

                Err errors ->
                    { model
                        | inputText = value
                        , inputErrors = List.map Parsers.deadEndToString errors
                    }

        GotHsluvTextInput name value ->
            let
                updateItem item =
                    case Parsers.parseHsluv value of
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
            { model
                | themeColorsByName =
                    Dict.update name (Maybe.map updateItem) model.themeColorsByName
            }

        GotHsluvRangeInput name component value ->
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
            { model
                | themeColorsByName =
                    Dict.update name (Maybe.map updateItem) model.themeColorsByName
            }

        Noop ->
            model
