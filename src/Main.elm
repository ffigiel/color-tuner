module Main exposing (main)

import Browser
import Dict
import Parsers
import Types exposing (..)
import View exposing (view)



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

        GotHsluvTextInput name hsl s ->
            let
                updateItem item =
                    let
                        component =
                            getThemeColorComponents hsl item.components

                        newComponent =
                            case String.toFloat s of
                                Just value ->
                                    { component
                                        | input = s
                                        , valid = True
                                        , value = value
                                    }

                                Nothing ->
                                    { component
                                        | input = s
                                        , valid = False
                                    }

                        newComponents =
                            setThemeColorComponents hsl newComponent item.components
                    in
                    { item
                        | newColor = colorFromComponents newComponents
                        , components = newComponents
                    }
            in
            { model
                | themeColorsByName =
                    Dict.update name (Maybe.map updateItem) model.themeColorsByName
            }

        GotHsluvRangeInput name hsl value ->
            let
                updateItem item =
                    let
                        component =
                            getThemeColorComponents hsl item.components

                        newComponent =
                            { component
                                | input = String.fromFloat value
                                , valid = True
                                , value = value
                            }

                        newComponents =
                            setThemeColorComponents hsl newComponent item.components
                    in
                    { item
                        | newColor = colorFromComponents newComponents
                        , components = newComponents
                    }
            in
            { model
                | themeColorsByName =
                    Dict.update name (Maybe.map updateItem) model.themeColorsByName
            }

        Noop ->
            model
