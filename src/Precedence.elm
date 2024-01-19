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


expression : Parser Expression
expression =
    binary precedences


precedences =
    [ [ "+", "-" ], [ "*", "/" ] ]


binary : List (List String) -> Parser Expression
binary l =
    case l of
        [] ->
            unary

        cur :: lower ->
            binary lower
                |> andThen
                    (\u ->
                        loop u
                            (\prev ->
                                oneOf
                                    (List.map
                                        (\s ->
                                            succeed (\next -> Loop (BinaryOp prev s next))
                                                |. symbol s
                                                |. spaces
                                                |= binary lower
                                        )
                                        cur
                                        ++ [ succeed (Done prev) ]
                                    )
                            )
                    )


unary : Parser Expression
unary =
    oneOf
        [ succeed (UnaryOp "-")
            |. symbol "-"
            |. spaces
            |= primary
            |. spaces
        , primary
        ]


primary : Parser Expression
primary =
    Parser.oneOf
        [ succeed EFloat |= Parser.float |. spaces
        , succeed Bracket
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol ")"
            |. spaces
        ]


mode : Mode
mode =
    { name = "Precedence"
    , evaluate = \s -> run (expression |. end) s |> Debug.toString
    , createTree = \_ -> Ok <| Tree { x = 0, y = 0, width = 0, height = 0, node = { width = 0, height = 0, label = "" } } []
    }
