module Precedence exposing (mode)

import Hierarchy
import Parser exposing (..)
import Shared exposing (LayoutNode, Mode, Node)
import Tree exposing (Tree(..))


type Expression
    = Bracket Expression
    | EFloat Float
    | UnaryOp String Expression
    | BinaryOp Expression String Expression


mode : Mode
mode =
    { name = "Precedence"
    , evaluate = \_ -> "Testing"
    , createTree = \_ -> Ok <| Tree { x = 0, y = 0, width = 0, height = 0, node = { width = 0, height = 0, label = "" } } []
    }
