module View exposing (view)

import Color exposing (Color)
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Round
import Types exposing (..)


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    H.div (HA.class "column" :: attrs) children


columnSmall : List (Attribute msg) -> List (Html msg) -> Html msg
columnSmall attrs children =
    H.div (HA.class "column -small" :: attrs) children


columnTight : List (Attribute msg) -> List (Html msg) -> Html msg
columnTight attrs children =
    H.div (HA.class "column -tight" :: attrs) children


row : List (Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    H.div (HA.class "row" :: attrs) children


rowSmall : List (Attribute msg) -> List (Html msg) -> Html msg
rowSmall attrs children =
    H.div (HA.class "row -small" :: attrs) children


rowTight : List (Attribute msg) -> List (Html msg) -> Html msg
rowTight attrs children =
    H.div (HA.class "row -tight" :: attrs) children


externalLink : List (Attribute msg) -> List (Html msg) -> Html msg
externalLink attrs children =
    H.a
        ([ HA.target "blank_"
         , HA.rel "noopener noreferrer"
         , HA.class "link"
         ]
            ++ attrs
        )
        children


view : Model -> Html Msg
view model =
    column [ HA.class "body" ]
        [ headerView
        , appView model
        , footerView
        ]


headerView : Html Msg
headerView =
    columnTight [ HA.class "appHeader" ]
        [ rowTight [ HA.class "appHeader_title", HA.id "title" ]
            ("Color\u{00A0}Tuner"
                |> String.toList
                |> List.map
                    (\c ->
                        H.span [ HA.style "position" "relative" ] [ H.text (String.fromChar c) ]
                    )
            )
        , H.p []
            [ H.text "🎨 Fine-tune your palette with "
            , externalLink
                [ HA.href "https://www.hsluv.org/" ]
                [ H.text "HSLuv" ]
            ]
        ]


appView : Model -> Html Msg
appView model =
    let
        themeColors =
            getThemeColors model
    in
    column
        [ HA.style "flex-grow" "1" ]
        [ row [ HA.class "-equal", HA.style "align-items" "stretch" ]
            [ inputView (model.inputErrors == []) model.inputText
            , outputView themeColors
            ]
        , if model.inputErrors /= [] then
            inputErrorsView model.inputErrors

          else
            themeColorsView themeColors model.zoom
        ]


inputErrorsView : List String -> Html Msg
inputErrorsView errors =
    let
        errorView e =
            H.p
                [ HA.class "errorMsg" ]
                [ H.text e ]
    in
    column []
        (List.map errorView errors)


themeColorsView : List ThemeColor -> Maybe HSL -> Html Msg
themeColorsView themeColors zoom =
    if themeColors == [] then
        H.p
            [ HA.class "errorMsg" ]
            [ H.text "No colors found." ]

    else
        columnSmall []
            [ themeColorsHeaderView zoom
            , columnTight []
                (themeAverageView themeColors zoom
                    :: List.map (themeColorView zoom) themeColors
                )
            ]


themeColorsHeaderView : Maybe HSL -> Html Msg
themeColorsHeaderView zoom =
    let
        components =
            List.map compView hslComponents

        compView comp =
            let
                isHidden =
                    zoom /= Nothing && zoom /= Just comp
            in
            rowSmall
                [ HA.classList
                    [ ( "equalFill", True )
                    , ( "header_hidden", isHidden )
                    ]
                , HA.style "justify-content" "center"
                ]
                [ H.span []
                    [ H.text <| hslToString comp
                    ]
                , H.button
                    [ HA.type_ "button"
                    , HA.classList [ ( "-active", zoom == Just comp ) ]
                    , HE.onClick (ToggleZoom comp)
                    ]
                    [ H.text "🔍" ]
                ]

        children =
            H.div [ HA.class "nameCol" ] [ H.text "Color" ]
                :: components
                ++ [ H.div [ HA.class "previewCol" ]
                        [ H.text "Preview " ]
                   ]
    in
    row [ HA.style "text-align" "center" ]
        children


themeAverageView : List ThemeColor -> Maybe HSL -> Html Msg
themeAverageView themeColors zoom =
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
                |> List.filterMap
                    (\( hsl, avg ) ->
                        if zoom == Nothing || zoom == Just hsl then
                            Just <| rangeInputView hsl avg

                        else
                            Nothing
                    )

        rangeInputView hsl avg =
            rowSmall [ HA.class "equalFill" ]
                [ themeColorComponentRangeInput
                    { hsl = hsl
                    , onChange = GotAverageRangeInput hsl avg
                    , value = avg
                    }
                , H.input
                    [ HA.class "componentInput"
                    , HA.readonly True
                    , HA.value <| Round.round 2 avg
                    ]
                    []
                ]

        children =
            H.div [ HA.class "nameCol", HA.style "text-align" "center" ]
                [ H.text "Average" ]
                :: rangeInputs
                ++ [ row
                        [ HA.class "previewCol"
                        ]
                        []
                   ]
    in
    row [] children


themeColorView : Maybe HSL -> ThemeColor -> Html Msg
themeColorView zoom item =
    let
        rangeInputs =
            hslComponents
                |> List.filterMap
                    (\hsl ->
                        if zoom == Nothing || zoom == Just hsl then
                            Just <| rangeInputView hsl

                        else
                            Nothing
                    )

        rangeInputView hsl =
            let
                component =
                    getThemeColorComponent hsl item.components
            in
            rowSmall [ HA.class "equalFill" ]
                [ themeColorComponentRangeInput
                    { hsl = hsl
                    , onChange = GotHsluvRangeInput item.name hsl
                    , value = component.normalizedValue
                    }
                , themeColorComponentInput
                    { onChange = GotHsluvTextInput item.name hsl
                    , value = component.input
                    , valid = component.valid
                    }
                ]

        children =
            H.div [ HA.class "nameCol" ]
                [ H.code [] [ H.text item.name ] ]
                :: rangeInputs
                ++ [ rowTight []
                        [ colorSwatch item.originalColor
                        , colorSwatch item.newColor
                        , textSwatch item.originalColor
                        , textSwatch item.newColor
                        ]
                   ]
    in
    row [] children


colorSwatch : Color -> Html Msg
colorSwatch color =
    H.div
        [ HA.style "background-color" <| Color.toCssString color
        , HA.class "previewSwatch"
        ]
        [ H.text "" ]


textSwatch : Color -> Html Msg
textSwatch color =
    H.div
        [ HA.style "color" <| Color.toCssString color
        , HA.class "previewSwatch"
        , HA.class "gridCenter"
        ]
        [ H.div [] [ H.text "Aa" ]
        ]


themeColorComponentInput :
    { onChange : String -> Msg
    , value : String
    , valid : Bool
    }
    -> Html Msg
themeColorComponentInput { onChange, value, valid } =
    let
        baseAttrs =
            [ HA.class "componentInput"
            , HA.type_ "text"
            , HE.onInput onChange
            , HA.value value
            ]

        attrs =
            if valid then
                baseAttrs

            else
                HA.class "inputWithError" :: baseAttrs
    in
    H.input
        attrs
        []


themeColorComponentRangeInput :
    { hsl : HSL, onChange : Float -> Msg, value : Float }
    -> Html Msg
themeColorComponentRangeInput { hsl, onChange, value } =
    let
        sliderMax =
            case hsl of
                Hue ->
                    360

                _ ->
                    100
    in
    H.input
        [ HA.type_ "range"
        , HE.onInput <| String.toFloat >> Maybe.withDefault value >> onChange
        , HA.min "0"
        , HA.max <| String.fromInt sliderMax
        , HA.step "0.01"
        , HA.value <| String.fromFloat value
        ]
        []


inputView : Bool -> String -> Html Msg
inputView valid value =
    let
        baseAttrs =
            [ HE.onInput GotInputText
            , HA.spellcheck False
            , HA.rows <| List.length <| String.lines <| value
            ]

        attrs =
            if valid then
                baseAttrs

            else
                HA.class "inputWithError" :: baseAttrs
    in
    columnTight []
        [ H.label [] [ H.text "Paste your color variables here" ]
        , H.textarea
            attrs
            [ H.text value ]
        ]


outputView : List ThemeColor -> Html Msg
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
    columnTight []
        [ H.label [] [ H.text "Output" ]
        , H.textarea
            [ HA.readonly True
            , HA.rows <| List.length <| String.lines <| outputText
            ]
            [ H.text outputText ]
        ]


footerView : Html Msg
footerView =
    row []
        [ externalLink
            [ HA.href "https://github.com/megapctr/color-tuner"
            ]
            [ H.text "Source" ]
        ]
