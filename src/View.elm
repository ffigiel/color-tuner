module View exposing (view)

import Color exposing (Color)
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Round
import Types exposing (HSL(..), Model, Msg(..), ThemeColor)


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    H.div (HA.class "column" :: attrs) children



{-
   columnSmall : List (Attribute msg) -> List (Html msg) -> Html msg
   columnSmall attrs children =
       H.div (HA.class "column -small" :: attrs) children
-}


columnTight : List (Attribute msg) -> List (Html msg) -> Html msg
columnTight attrs children =
    H.div (HA.class "column -tight" :: attrs) children


columnCell : List (Attribute msg) -> List (Html msg) -> Html msg
columnCell attrs children =
    H.div (HA.class "columnCell" :: attrs) children


columnCellText : String -> Html msg
columnCellText text =
    columnCell [] [ H.p [] [ H.text text ] ]


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
            Types.getThemeColors model
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
        H.div
            [ HA.class "appControls" ]
            [ colColorNames themeColors
            , colInputs zoom themeColors
            , colPreview themeColors
            ]


colColorNames : List ThemeColor -> Html Msg
colColorNames themeColors =
    columnTight []
        ([ columnCellText "Color"
         , columnCellText "Average"
         ]
            ++ List.map
                (\c ->
                    columnCell
                        [ HA.class "nameCell"
                        ]
                        [ H.code [] [ H.text c.name ] ]
                )
                themeColors
        )


colInputs : Maybe HSL -> List ThemeColor -> Html Msg
colInputs zoom themeColors =
    H.div []
        [ row [ HA.class "-equal" ]
            (List.map (colComponentHeader zoom) Types.hslComponents)
        , row [ HA.class "-equal" ]
            (List.map (colComponent zoom themeColors) Types.hslComponents)
        ]


colComponentHeader : Maybe HSL -> HSL -> Html Msg
colComponentHeader zoom comp =
    componentHeader zoom comp


colComponent : Maybe HSL -> List ThemeColor -> HSL -> Html Msg
colComponent zoom themeColors comp =
    if zoom /= Nothing && zoom /= Just comp then
        H.text ""

    else
        let
            compAvg =
                List.foldl
                    (\color sum ->
                        color
                            |> .components
                            |> Types.getThemeColorComponent comp
                            |> .value
                            |> (+) sum
                    )
                    0
                    themeColors
                    |> (\sum -> sum / toFloat (List.length themeColors))
        in
        H.div
            [ HA.class "appControls_colorComponent" ]
            [ colComponentSliders comp compAvg themeColors
            , colComponentInputs comp compAvg themeColors
            ]


colComponentSliders : HSL -> Float -> List ThemeColor -> Html Msg
colComponentSliders comp compAvg themeColors =
    columnTight []
        (themeColorComponentRangeInput
            { hsl = comp
            , onChange = GotAverageRangeInput comp compAvg
            , value = compAvg
            }
            :: List.map
                (\color ->
                    let
                        component =
                            Types.getThemeColorComponent comp color.components
                    in
                    themeColorComponentRangeInput
                        { hsl = comp
                        , onChange = GotHsluvRangeInput color.name comp
                        , value = component.normalizedValue
                        }
                )
                themeColors
        )


componentHeader : Maybe HSL -> HSL -> Html Msg
componentHeader zoom comp =
    let
        isHidden =
            zoom /= Nothing && zoom /= Just comp
    in
    columnCell
        []
        [ rowSmall
            [ HA.classList
                [ ( "header_hidden", isHidden )
                ]
            ]
            [ H.span []
                [ H.text <| Types.hslToString comp
                ]
            , H.button
                [ HA.type_ "button"
                , HA.classList [ ( "-active", zoom == Just comp ) ]
                , HE.onClick (ToggleZoom comp)
                ]
                [ H.text "🔍" ]
            ]
        ]


colComponentInputs : HSL -> Float -> List ThemeColor -> Html Msg
colComponentInputs comp compAvg themeColors =
    columnTight []
        (columnCell []
            [ H.code []
                [ H.text <| Round.round 1 compAvg ]
            ]
            :: List.map
                (\color ->
                    let
                        component =
                            Types.getThemeColorComponent comp color.components
                    in
                    themeColorComponentInput
                        { onChange = GotHsluvTextInput color.name comp
                        , value = component.input
                        , valid = component.valid
                        }
                )
                themeColors
        )


colPreview : List ThemeColor -> Html Msg
colPreview themeColors =
    columnTight []
        (columnCellText "Preview"
            :: columnCellText ""
            :: List.map
                (\c ->
                    rowTight []
                        [ colorSwatch c.originalColor
                        , colorSwatch c.newColor
                        ]
                )
                themeColors
        )


colorSwatch : Color -> Html Msg
colorSwatch color =
    H.div
        [ HA.style "background-color" <| Color.toCssString color
        , HA.class "previewSwatch"
        ]
        [ H.text "" ]


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
    columnCell []
        [ H.input
            attrs
            []
        ]


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
    columnCell []
        [ H.input
            [ HA.type_ "range"
            , HE.onInput <| String.toFloat >> Maybe.withDefault value >> onChange
            , HA.min "0"
            , HA.max <| String.fromInt sliderMax
            , HA.step "0.1"
            , HA.value <| String.fromFloat value
            ]
            []
        ]


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
                ++ Types.rgbToString item.newColor
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
