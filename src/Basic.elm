module Basic exposing (Expression, evaluate, layoutTree, mode, parse)

import Hierarchy
import Parser exposing (..)
import Shared exposing (LayoutNode, Mode, Node, ParserResult)
import Tree exposing (Tree(..))


type Expression
    = EFloat Float
    | Bracket Expression
    | Add Expression Expression
    | Subtract Expression Expression
    | Multiply Expression Expression
    | Divide Expression Expression


operatorAux : (Expression -> Expression -> Expression) -> String -> Parser (Expression -> Expression -> Expression)
operatorAux o s =
    succeed o
        |. symbol s


operator : Parser Expression
operator =
    succeed (\a o b -> o a b)
        |= oneOf [ bracketExpression, Parser.map EFloat float ]
        |. spaces
        |= oneOf
            [ operatorAux Add "+", operatorAux Subtract "-", operatorAux Divide "/", operatorAux Multiply "*" ]
        |. spaces
        |= oneOf [ bracketExpression, Parser.map EFloat float ]
        |. spaces


bracketExpression : Parser Expression
bracketExpression =
    succeed Bracket
        |. symbol "("
        |. spaces
        |= lazy (\_ -> operator)
        |. spaces
        |. symbol ")"
        |. spaces


evaluate : Expression -> Float
evaluate e =
    case e of
        EFloat f ->
            f

        Bracket n ->
            evaluate n

        Add a b ->
            evaluate a + evaluate b

        Subtract a b ->
            evaluate a - evaluate b

        Multiply a b ->
            evaluate a * evaluate b

        Divide a b ->
            evaluate a / evaluate b


parse : String -> ParserResult Expression
parse s =
    run (bracketExpression |. end) s


toTree : Expression -> Tree Node
toTree e =
    let
        small =
            { width = 20, height = 20, label = "" }

        big =
            { width = 40, height = 40, label = "" }
    in
    case e of
        EFloat f ->
            Tree { small | label = String.fromFloat f } []

        Bracket ep ->
            Tree { big | label = "()" } [ toTree ep ]

        Add a b ->
            Tree { big | label = "+" } [ toTree a, toTree b ]

        Subtract a b ->
            Tree { big | label = "-" } [ toTree a, toTree b ]

        Multiply a b ->
            Tree { big | label = "*" } [ toTree a, toTree b ]

        Divide a b ->
            Tree { big | label = "/" } [ toTree a, toTree b ]


toLayout : Tree Node -> Tree LayoutNode
toLayout t =
    Hierarchy.tidy
        [ Hierarchy.nodeSize (\{ width, height } -> ( width, height ))
        , Hierarchy.none
        , Hierarchy.parentChildMargin 10
        , Hierarchy.peerMargin 10
        ]
        t


mode : Mode
mode =
    { name = "Basic"
    , evaluate =
        \s ->
            case parse s of
                Ok res ->
                    evaluate res |> String.fromFloat

                Err _ ->
                    "Error"
    , createTree =
        \s ->
            case Result.map layoutTree <| parse s of
                Ok res ->
                    Just res

                Err _ ->
                    Nothing
    }


layoutTree : Expression -> Tree LayoutNode
layoutTree t =
    t |> toTree |> toLayout
