module Main exposing (Model, Msg(..), init, main, update, view)

-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)

import Basic
import Browser
import Dict
import Display exposing (treeCanvas)
import Element exposing (column, el, fill, focused, height, html, htmlAttribute, padding, paddingXY, paragraph, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, multiline)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Precedence
import Shared exposing (LayoutNode, Mode, ParserResult)
import Tree exposing (Tree(..))



-- MAIN


modes =
    [ Basic.mode, Precedence.mode ]


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { content : String
    , result : String
    , tree : ParserResult (Tree LayoutNode)
    , mode : Mode
    }


init : Model
init =
    { content = ""
    , result = ""
    , tree = Err []
    , mode = Basic.mode
    }


type Msg
    = EditCode String
    | None
    | Run
    | ChangeMode Mode


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditCode str ->
            { model | content = str }

        None ->
            model

        ChangeMode m ->
            { init | mode = m }

        Run ->
            { model
                | tree = model.mode.createTree model.content
                , result = model.mode.evaluate model.content
            }


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
                , onChange = EditCode
                , placeholder = Nothing
                , label = labelHidden "Code"
                , spellcheck = False
                }

        resultBox =
            paragraph
                [ Background.color (rgb 0.1 0.1 0.1)
                , height fill
                , width fill
                ]
                [ text model.result ]

        runButton =
            button [ padding 4 ] { onPress = Just Run, label = text "Run" }

        buttonRow =
            row [ width fill, Background.color (rgb 0.3 0.3 0.3) ] [ runButton ]

        navbar =
            row
                [ width fill
                , padding 2
                , Background.color (rgb 0.2 0.2 0.2)
                ]
                -- TODO Change to button
                (List.map (\m -> button [ paddingXY 2 0 ] { onPress = Just <| ChangeMode m, label = text m.name }) modes)
    in
    -- Main Layout
    --
    --
    Element.layout
        [ font
        , Font.color (rgb 0.7 0.7 0.7)
        , Font.size 16
        ]
        (column [ width fill, height fill ]
            [ navbar
            , row [ width fill, height fill ]
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
                , column [ width fill, height fill ] [ html (treeCanvas model.tree) ]
                ]
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
