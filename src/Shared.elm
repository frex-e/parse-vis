module Shared exposing (LayoutNode, Mode, Node, ParserResult)

import Parser exposing (DeadEnd)
import Tree exposing (Tree(..))


type alias Mode =
    { name : String
    , evaluate : String -> String
    , createTree : String -> ParserResult (Tree LayoutNode)
    }


type alias Node =
    { width : Float
    , height : Float
    , label : String
    }


type alias LayoutNode =
    { x : Float, y : Float, width : Float, height : Float, node : Node }


type alias ParserResult e =
    Result (List DeadEnd) e
