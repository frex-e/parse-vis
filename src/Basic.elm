module Basic exposing (parse)

import Debug exposing (toString)
import Parser exposing (..)


type CST
    = MyInt Int
    | Bracket CST
    | Add CST CST
    | Subtract CST CST
    | Multiply CST CST
    | Divide CST CST


operatorAux : (Float -> Float -> Float) -> String -> Parser (Float -> Float -> Float)
operatorAux o s =
    succeed o
        |. symbol s


operator : Parser Float
operator =
    succeed (\a o b -> o a b)
        |= oneOf [ bracketExpression, float ]
        |. spaces
        |= oneOf
            [ operatorAux (+) "+", operatorAux (-) "-", operatorAux (/) "/", operatorAux (*) "*" ]
        |. spaces
        |= oneOf [ bracketExpression, float ]
        |. spaces


bracketExpression : Parser Float
bracketExpression =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> operator)
        |. spaces
        |. symbol ")"
        |. spaces


parse : String -> String
parse s =
    case run bracketExpression s of
        Ok res ->
            String.fromFloat res

        Err deadEnds ->
            deadEndsToString deadEnds
