module Display exposing (..)

import Color
import Html exposing (Html)
import Html.Attributes
import Path
import Shape
import Shared exposing (LayoutNode, ParserResult)
import Tree exposing (Tree(..))
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (fill, height, rx, stroke, textAnchor, transform, width, x, y)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..))


viewTree : Tree LayoutNode -> Svg a
viewTree tree =
    g [ transform [ Translate 200 0 ] ]
        [ tree
            |> Tree.links
            |> List.map
                (\( from, to ) -> Shape.bumpYCurve [ ( from.x + from.width / 2, from.y + from.height ), ( to.x + to.width / 2, to.y ) ])
            |> (\path ->
                    Path.element path
                        [ TypedSvg.Attributes.fill PaintNone
                        , stroke (Paint (Color.rgb 0 0 0))
                        ]
               )
        , tree
            |> Tree.toList
            |> List.map
                (\item ->
                    g [ transform [ Translate item.x item.y ] ]
                        [ rect
                            [ InPx.rx 5
                            , InPx.width item.width
                            , InPx.height item.height

                            -- , InPx.x item.x
                            -- , InPx.y item.y
                            , fill (Paint (Color.rgb 0.5 0.5 0.5))
                            , stroke (Paint (Color.rgb 1 1 1))
                            ]
                            []
                        , text_ [ InPx.x (item.width / 2), InPx.y (item.height / 2), textAnchor AnchorMiddle ] [ text item.node.label ]
                        ]
                )
            |> g []
        ]


treeCanvas : Maybe (Tree LayoutNode) -> Html msg
treeCanvas tree =
    svg
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        ]
        (case tree of
            Just t ->
                [ viewTree t ]

            Nothing ->
                []
        )
