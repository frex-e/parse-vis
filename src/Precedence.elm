module Precedence exposing (mode)

import Hierarchy
import Parser exposing (..)
import Shared exposing (LayoutNode, Mode, Node)
import Tree exposing (Tree(..))


type Expression
    = Bracket Expression
    | EFloat Float
    | UnaryExp UnaryOperation Expression
    | BinaryExp Expression BinaryOperation Expression


type UnaryOperation
    = Negate


type BinaryOperation
    = Add
    | Subtract
    | Multiply
    | Divide


toOp : BinaryOperation -> (Float -> Float -> Float)
toOp op =
    case op of
        Add ->
            (+)

        Subtract ->
            (-)

        Multiply ->
            (*)

        Divide ->
            (/)


printBinaryOp : BinaryOperation -> String
printBinaryOp op =
    case op of
        Add ->
            "+"

        Subtract ->
            "-"

        Multiply ->
            "*"

        Divide ->
            "/"


expression : Parser Expression
expression =
    binary precedences


precedences =
    [ [ ( Add, "+" ), ( Subtract, "-" ) ], [ ( Multiply, "*" ), ( Divide, "/" ) ] ]


binary : List (List ( BinaryOperation, String )) -> Parser Expression
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
                                        (\( op, str ) ->
                                            succeed (\next -> Loop (BinaryExp prev op next))
                                                |. symbol str
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
        [ succeed (UnaryExp Negate)
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


evalTree : Expression -> Float
evalTree e =
    case e of
        Bracket ep ->
            evalTree ep

        EFloat f ->
            f

        UnaryExp Negate ep ->
            negate <| evalTree ep

        BinaryExp a op b ->
            toOp op (evalTree a) (evalTree b)


toTreeAux : Expression -> Tree Node
toTreeAux e =
    case e of
        Bracket ep ->
            Tree { width = 30, height = 30, label = "()" } [ toTreeAux ep ]

        EFloat f ->
            Tree { width = 30, height = 30, label = String.fromFloat f } []

        UnaryExp Negate ep ->
            Tree { width = 30, height = 30, label = "-" } [ toTreeAux ep ]

        BinaryExp a op b ->
            Tree { width = 30, height = 30, label = printBinaryOp op } [ toTreeAux a, toTreeAux b ]


toTree : Expression -> Tree LayoutNode
toTree t =
    Hierarchy.tidy
        [ Hierarchy.nodeSize (\{ width, height } -> ( width, height ))
        , Hierarchy.none
        , Hierarchy.parentChildMargin 10
        , Hierarchy.peerMargin 10
        ]
        (toTreeAux t)


mode : Mode
mode =
    { name = "Precedence"
    , evaluate =
        \s ->
            case run (expression |. end) s of
                Ok res ->
                    evalTree res |> String.fromFloat

                Err err ->
                    Debug.toString err

    -- , createTree = \_ -> Just <| Tree { x = 0, y = 0, width = 0, height = 0, node = { width = 0, height = 0, label = "" } } []
    , createTree =
        \s ->
            case run (expression |. end) s of
                Ok res ->
                    Just <| toTree res

                Err _ ->
                    Nothing
    }
