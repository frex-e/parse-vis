module Main exposing (Model, Msg(..), init, main, update, view)

-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)

import Basic exposing (parse)
import Browser
import Element exposing (column, el, fill, focused, height, htmlAttribute, padding, px, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, labelHidden, multiline)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Parser



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { content : String
    , result : String
    }


init : Model
init =
    { content = ""
    , result = ""
    }


type Msg
    = Change String
    | None
    | Run


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change str ->
            { model | content = str }

        None ->
            model

        Run ->
            { model | result = Basic.parse model.content }


view : Model -> Html Msg
view model =
    let
        font =
            Font.family [ Font.typeface "Consolas", Font.monospace ]

        textBox =
            multiline
                [ Border.width 0
                , focused []
                , Background.color (rgb 0.4 0.4 0.4)
                , Border.rounded 0
                , height fill
                , onShiftEnter Run
                ]
                { text = model.content
                , onChange = Change
                , placeholder = Nothing
                , label = labelHidden "Code"
                , spellcheck = False
                }

        resultBox =
            el [ Background.color (rgb 0.1 0.1 0.1), height fill, width fill ] (text model.result)

        runButton =
            button [ padding 4 ] { onPress = Just Run, label = text "Run" }

        buttonRow =
            row [ width fill, Background.color (rgb 0.3 0.3 0.3) ] [ runButton ]
    in
    -- Main Layout
    --
    --
    Element.layout
        [ font
        , Font.color (rgb 0.7 0.7 0.7)
        , Font.size 16
        ]
        (row [ width fill, height fill ]
            [ column
                [ width fill
                , height fill
                , Border.widthEach { left = 0, right = 2, top = 0, bottom = 0 }
                , Border.color (rgb 0 0 0)
                ]
                [ column [ width fill, height fill ]
                    [ textBox
                    , buttonRow
                    ]
                , resultBox
                ]
            , column [ width fill, height fill ] []
            ]
        )


onShiftEnter : Msg -> Element.Attribute Msg
onShiftEnter msg =
    let
        keyExtractor =
            Decode.map2
                (\key shift ->
                    if key == 13 && shift then
                        ( msg, True )

                    else
                        ( None, False )
                )
                (Decode.field "keyCode" Decode.int)
                (Decode.field "shiftKey" Decode.bool)
    in
    keyExtractor |> Html.Events.preventDefaultOn "keydown" |> htmlAttribute
