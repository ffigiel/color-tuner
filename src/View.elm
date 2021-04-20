module View exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as HA
import Round
import Types exposing (..)


rem : number
rem =
    16


errColor : Color
errColor =
    rgb255 220 0 0


sliderRailColor : Color
sliderRailColor =
    rgb255 220 220 220


outputBackgroundColor : Color
outputBackgroundColor =
    rgb255 245 245 245


linkColor : Color
linkColor =
    rgb255 0 0 220


spacingSmall : Attribute Msg
spacingSmall =
    spacing <| rem


spacingDefault : Attribute Msg
spacingDefault =
    spacing <| 2 * rem


fontMonospace : Attribute Msg
fontMonospace =
    Font.family [ Font.monospace ]


paddingDefault : Attribute Msg
paddingDefault =
    padding <| 2 * rem


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
            , width (fill |> maximum (100 * rem))
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
    let
        themeColors =
            getThemeColors model
    in
    column
        [ spacingDefault
        , width fill
        , Region.mainContent
        ]
        [ row [ spacingDefault, width fill ]
            [ inputView (model.inputErrors == []) model.inputText
            , outputView themeColors
            ]
        , if model.inputErrors /= [] then
            inputErrorsView model.inputErrors

          else
            themeColorsView themeColors
        ]


inputErrorsView : List String -> Element Msg
inputErrorsView errors =
    let
        errorView e =
            paragraph
                [ Font.color errColor ]
                [ text e ]
    in
    column [ width fill ]
        (List.map errorView errors)


themeColorsView : List ThemeColor -> Element Msg
themeColorsView themeColors =
    if themeColors == [] then
        text "No colors found."

    else
        column [ spacingSmall, width fill ]
            [ themeColorsHeaderView
            , column [ width fill ]
                (themeAverageView themeColors
                    :: List.map themeColorView themeColors
                )
            ]


nameColWidth : number
nameColWidth =
    7 * rem


themeColorsHeaderView : Element Msg
themeColorsHeaderView =
    let
        previewColWidth =
            4 * colorSwatchWidth
    in
    row [ spacingDefault, width fill, Font.center ]
        [ el [ width <| px nameColWidth ] (text "Color")
        , el [ width fill ] (text "Hue")
        , el [ width fill ] (text "Saturation")
        , el [ width fill ] (text "Lightness")
        , el [ width <| px previewColWidth ] (text "Preview")
        ]


themeAverageView : List ThemeColor -> Element Msg
themeAverageView themeColors =
    let
        numColors =
            toFloat <| List.length themeColors

        ( totH, totS, totL ) =
            List.foldl
                (\color ( h, s, l ) ->
                    ( h + color.components.h.value
                    , s + color.components.s.value
                    , l + color.components.l.value
                    )
                )
                ( 0, 0, 0 )
                themeColors

        ( avgH, avgS, avgL ) =
            ( totH / numColors, totS / numColors, totL / numColors )

        rangeInputs =
            [ ( Hue, avgH ), ( Saturation, avgS ), ( Lightness, avgL ) ]
                |> List.map
                    (\( hsl, avg ) ->
                        row [ spacingSmall, width fill ]
                            [ themeColorComponentRangeInput
                                { hsl = hsl
                                , onChange = GotAverageRangeInput hsl avg
                                , value = avg
                                }
                            , el
                                [ width <| px componentInputWidth
                                ]
                                (text <| Round.round 2 avg)
                            ]
                    )

        children =
            el [ width <| px nameColWidth ]
                (text "Average")
                :: rangeInputs
                ++ [ row [ width <| px (colorSwatchWidth * 4) ] []
                   ]
    in
    row
        [ spacingDefault
        , width fill
        , height <| px colorSwatchHeight
        ]
        children


themeColorView : ThemeColor -> Element Msg
themeColorView item =
    let
        rangeInputs =
            [ Hue, Saturation, Lightness ]
                |> List.map
                    (\hsl ->
                        let
                            component =
                                getThemeColorComponents hsl item.components
                        in
                        row [ spacingSmall, width fill ]
                            [ themeColorComponentRangeInput
                                { hsl = hsl
                                , onChange = GotHsluvRangeInput item.name hsl
                                , value = component.value
                                }
                            , themeColorComponentInput
                                { label = hslToString hsl
                                , onChange = GotHsluvTextInput item.name hsl
                                , value = component.input
                                , valid = component.valid
                                }
                            ]
                    )

        children =
            el [ width <| px nameColWidth ]
                (text item.name)
                :: rangeInputs
                ++ [ row []
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


colorSwatchWidth : number
colorSwatchWidth =
    4 * rem


colorSwatchHeight : number
colorSwatchHeight =
    3 * rem


colorSwatch : Color -> Element Msg
colorSwatch color =
    el
        [ Background.color color
        , width <| px colorSwatchWidth
        , height <| px colorSwatchHeight
        ]
        (text "")


textSwatch : Color -> Element Msg
textSwatch color =
    el
        [ Font.color color
        , width <| px colorSwatchWidth
        , height <| px colorSwatchHeight
        , centerX
        , centerY
        ]
        (el [ centerX, centerY ] (text "Aa"))


componentInputWidth : number
componentInputWidth =
    5 * rem


themeColorComponentInput :
    { label : String
    , onChange : String -> Msg
    , value : String
    , valid : Bool
    }
    -> Element Msg
themeColorComponentInput { label, onChange, value, valid } =
    let
        baseAttrs =
            [ width <| px componentInputWidth
            ]

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


themeColorComponentRangeInput :
    { hsl : HSL, onChange : Float -> Msg, value : Float }
    -> Element Msg
themeColorComponentRangeInput { hsl, onChange, value } =
    let
        sliderMax =
            case hsl of
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
                , Background.color sliderRailColor
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = onChange
        , label = Input.labelHidden <| hslToString hsl
        , min = 0
        , max = sliderMax
        , step = Just 0.01
        , value = value
        , thumb =
            Input.defaultThumb
        }


inputView : Bool -> String -> Element Msg
inputView valid value =
    let
        attrs =
            if valid then
                []

            else
                [ Border.color errColor ]
    in
    Input.multiline
        (width fill :: attrs)
        { onChange = GotInputText
        , text = value
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Input")
        , spellcheck = False
        }


outputView : List ThemeColor -> Element Msg
outputView colors =
    let
        outputText =
            colors
                |> List.map toCssValue
                |> String.join "\n"

        toCssValue item =
            item.name
                ++ ": "
                ++ rgbToString item.newColor
                ++ ";"
    in
    Input.multiline
        [ width fill
        , height fill
        , htmlAttribute <| HA.attribute "readonly" ""
        , Background.color outputBackgroundColor
        ]
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
        , Region.footer
        ]
        [ newTabLink
            [ Font.color linkColor, Font.underline ]
            { url = "https://github.com/megapctr/color-tuner"
            , label = text "Source"
            }
        ]
