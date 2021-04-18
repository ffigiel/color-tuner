module View exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as HA
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
                (List.map themeColorView themeColors)
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


themeColorView : ThemeColor -> Element Msg
themeColorView item =
    let
        rangeInputs =
            [ Hue, Saturation, Lightness ]
                |> List.map
                    (\c ->
                        hsluvRangeInput
                            { component = c
                            , onChange = GotHsluvRangeInput item.name c
                            , value = getHsluvComponent c item.hsluvComponents
                            }
                    )

        children =
            [ el [ width <| px (7 * rem) ]
                (text item.name)
            , hsluvInput
                { label = item.name
                , onChange = GotHsluvTextInput item.name
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
                , Background.color sliderRailColor
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
