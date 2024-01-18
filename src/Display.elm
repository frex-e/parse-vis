module Display exposing (..)

import Color
import Html exposing (Html)
import Html.Attributes
import Path
import Shape
import Shared exposing (LayoutNode, ParserResult)
import Tree exposing (Tree(..))
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (fill, height, rx, stroke, transform, width, x, y)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


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
                    rect
                        [ InPx.rx 5
                        , InPx.width item.width
                        , InPx.height item.height
                        , InPx.x item.x
                        , InPx.y item.y
                        , fill (Paint (Color.rgb 0.5 0.5 0.5))
                        , stroke (Paint (Color.rgb 1 1 1))
                        ]
                        []
                )
            |> g []
        ]


treeCanvas : ParserResult (Tree LayoutNode) -> Html msg
treeCanvas tree =
    svg
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        ]
        (case tree of
            Ok t ->
                [ viewTree t ]

            Err _ ->
                []
        )
