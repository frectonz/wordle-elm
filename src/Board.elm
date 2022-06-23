module Board exposing (..)

import InputWord exposing (InputWord, mapInputWordToString)
import List.Extra
import Vector5 exposing (Vector5)
import Vector6 exposing (Vector6)


type alias Board =
    Vector6 Row


type alias Row =
    Vector5 Letter


type Letter
    = Misplaced Char
    | Correct Char
    | Incorrect Char
    | Empty


initalBoard : Board
initalBoard =
    Vector6.repeat (Vector5.repeat Empty)


boardTo2DList : Board -> List ( Vector6.Index, List Letter )
boardTo2DList board =
    board
        |> Vector6.map Vector5.toList
        |> Vector6.toIndexedList


findFirstEmptyRow : Board -> Maybe Vector6.Index
findFirstEmptyRow board =
    board
        |> boardTo2DList
        |> List.Extra.find
            (\( _, row ) ->
                row
                    |> List.all (\l -> l == Empty)
            )
        |> Maybe.map Tuple.first


checkIfLetterExistsInSecretWord : Char -> String -> Bool
checkIfLetterExistsInSecretWord w secretWord =
    List.any (\x -> x == w) (String.toList secretWord)


changeInputWordToBoardRow : InputWord -> String -> Vector5 Letter
changeInputWordToBoardRow inputWord secretWord =
    List.map2
        (\inputLetter correctLetter ->
            if inputLetter == correctLetter then
                Correct inputLetter

            else if checkIfLetterExistsInSecretWord inputLetter secretWord then
                Misplaced inputLetter

            else
                Incorrect inputLetter
        )
        (String.toList (mapInputWordToString inputWord))
        (String.toList secretWord)
        |> Vector5.fromListWithDefault Empty
        |> Tuple.second


addInputWordToBoard : InputWord -> String -> Board -> Vector6.Index -> Board
addInputWordToBoard inputWord secretWord board index =
    board
        |> Vector6.set index (changeInputWordToBoardRow inputWord secretWord)


boardToList : Board -> List (List Letter)
boardToList board =
    board
        |> Vector6.map Vector5.toList
        |> Vector6.toList
