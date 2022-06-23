module InputWord exposing (..)

import List.Extra
import Vector5 exposing (Vector5)
import Words exposing (dictionary)


type alias InputWord =
    Vector5 InputChar


type InputChar
    = Filled Char
    | Unfilled


initialInputWord : InputWord
initialInputWord =
    Vector5.repeat Unfilled


addCharToInputWord : Char -> InputWord -> InputWord
addCharToInputWord char inputWord =
    inputWord
        |> Vector5.toIndexedList
        -- find the first unfilled slot
        |> List.Extra.find (\( _, x ) -> x == Unfilled)
        |> Maybe.map Tuple.first
        |> Maybe.map
            (\i ->
                Vector5.set i (Filled char) inputWord
            )
        |> Maybe.withDefault
            inputWord


backspaceInputtWord : InputWord -> InputWord
backspaceInputtWord inputWord =
    inputWord
        |> Vector5.toIndexedList
        -- remove all unfilled slots
        |> List.filter (\( _, x ) -> x /= Unfilled)
        -- get the last filled slot
        |> List.Extra.last
        |> Maybe.map Tuple.first
        |> Maybe.map
            (\i ->
                Vector5.set i Unfilled inputWord
            )
        |> Maybe.withDefault
            inputWord


isInputWordCompleted : InputWord -> Bool
isInputWordCompleted inputWord =
    inputWord
        |> Vector5.toList
        |> List.all (\x -> x /= Unfilled)


mapInputWordToString : InputWord -> String
mapInputWordToString inputWord =
    inputWord
        |> Vector5.map
            (\l ->
                case l of
                    Filled char ->
                        String.fromChar char

                    Unfilled ->
                        ""
            )
        |> Vector5.foldr (++) ""
        |> String.toLower


isInputWordValid : InputWord -> Bool
isInputWordValid inputWord =
    List.member (mapInputWordToString inputWord) dictionary
