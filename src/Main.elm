module Main exposing (..)

import Board
    exposing
        ( Board
        , Letter(..)
        , addInputWordToBoard
        , boardToList
        , findFirstEmptyRow
        , initalBoard
        )
import Browser
import Browser.Events exposing (onKeyUp)
import Html exposing (Html, button, div, h1, header, text)
import Html.Attributes exposing (class)
import Html.Events as HtmlEvents
import InputWord
    exposing
        ( InputChar(..)
        , InputWord
        , addCharToInputWord
        , backspaceInputtWord
        , initialInputWord
        , isInputWordCompleted
        , isInputWordValid
        )
import Json.Decode as Decode
import List.Extra as ListExtra
import Time
import Utils exposing (flatten2D)
import Vector5


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    , inputWord : InputWord
    , secretWord : String
    , displayToast : Bool
    }


type Msg
    = Character Char
    | CloseToast Time.Posix
    | Backspace
    | SubmitInputWord
    | NoOp


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { board = initalBoard
      , inputWord = initialInputWord
      , secretWord = "cigar"
      , displayToast = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyUp keyDecoder
        , Time.every 5000 CloseToast
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            if char == ' ' then
                NoOp

            else
                Character (Char.toUpper char)

        _ ->
            if string == "Backspace" then
                Backspace

            else if string == "Enter" then
                SubmitInputWord

            else
                NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CloseToast _ ->
            ( { model | displayToast = False }, Cmd.none )

        Character char ->
            ( { model
                | inputWord = addCharToInputWord char model.inputWord
              }
            , Cmd.none
            )

        Backspace ->
            ( { model
                | inputWord = backspaceInputtWord model.inputWord
              }
            , Cmd.none
            )

        SubmitInputWord ->
            if isInputWordValid model.inputWord then
                case findFirstEmptyRow model.board of
                    Just index ->
                        ( { model
                            | board = addInputWordToBoard model.inputWord model.secretWord model.board index
                            , inputWord = initialInputWord
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        --game over
                        ( model, Cmd.none )

            else
                ( { model
                    | displayToast =
                        isInputWordCompleted model.inputWord
                            && not (isInputWordValid model.inputWord)
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen font-mono" ]
        [ viewHeader
        , viewBoard model
        , viewToast model.displayToast
        , viewKeyboard model.board
        ]


viewHeader : Html Msg
viewHeader =
    header [ class "border-b border-black" ]
        [ h1 [ class "text-3xl text-center py-4 font-bold" ] [ text "WORDLE" ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        boardList =
            boardToList model.board

        previous =
            boardList
                |> ListExtra.takeWhile
                    (List.all (\x -> x /= Empty))
                |> flatten2D
                |> List.map viewLetter

        currentRowHtml =
            model.inputWord
                |> Vector5.toList
                |> List.map
                    (\x ->
                        div [ class "text-4xl shadow-sm shadow-blue-400 flex justify-center items-center" ]
                            [ text
                                (case x of
                                    Filled c ->
                                        String.fromChar c

                                    Unfilled ->
                                        " "
                                )
                            ]
                    )

        next =
            boardList
                |> ListExtra.takeWhileRight
                    (\row ->
                        row |> List.all (\x -> x == Empty)
                    )
                |> flatten2D
                |> List.map viewLetter
    in
    div
        [ class "grid grid-cols-5 grid-rows-6 my-10 mx-auto gap-4 w-[400px] h-[400px]"
        ]
        (previous ++ currentRowHtml ++ next)


viewLetter : Letter -> Html Msg
viewLetter letter =
    let
        bgColor =
            case letter of
                Misplaced _ ->
                    "bg-yellow-500 text-white"

                Correct _ ->
                    "bg-green-500 text-white"

                Incorrect _ ->
                    "bg-gray-500 text-white"

                Empty ->
                    "bg-white"
    in
    div
        [ class
            (bgColor ++ " text-2xl shadow-sm shadow-blue-400 flex justify-center items-center")
        ]
        [ text
            (case letter of
                Misplaced char ->
                    String.fromChar char |> String.toUpper

                Correct char ->
                    String.fromChar char |> String.toUpper

                Incorrect char ->
                    String.fromChar char |> String.toUpper

                Empty ->
                    " "
            )
        ]


viewToast : Bool -> Html Msg
viewToast displayToast =
    let
        visible =
            if displayToast then
                "visible"

            else
                "hidden"
    in
    div
        [ class
            (visible ++ " fixed top-[55vh] left-1/2 -translate-x-1/2 bg-gray-500 py-2 px-4 text-white rounded-lg shadow-lg")
        ]
        [ text "Not in word list" ]


viewKeyboard : Board -> Html Msg
viewKeyboard board =
    div [ class "w-[600px] mx-auto grid grid-rows-3 gap-3" ]
        [ div [ class "grid grid-cols-10 gap-2" ]
            ([ ( "Q", Character 'Q' )
             , ( "W", Character 'W' )
             , ( "E", Character 'E' )
             , ( "R", Character 'R' )
             , ( "T", Character 'T' )
             , ( "Y", Character 'Y' )
             , ( "U", Character 'U' )
             , ( "I", Character 'I' )
             , ( "O", Character 'O' )
             , ( "P", Character 'P' )
             ]
                |> List.map (viewKeyboardLetter board)
            )
        , div [ class "grid grid-cols-9 gap-2 px-4" ]
            ([ ( "A", Character 'A' )
             , ( "S", Character 'S' )
             , ( "D", Character 'D' )
             , ( "F", Character 'F' )
             , ( "G", Character 'G' )
             , ( "H", Character 'H' )
             , ( "J", Character 'J' )
             , ( "K", Character 'K' )
             , ( "L", Character 'L' )
             ]
                |> List.map (viewKeyboardLetter board)
            )
        , div [ class "grid grid-cols-9 gap-2" ]
            ([ ( "ENTER", SubmitInputWord )
             , ( "Z", Character 'Z' )
             , ( "X", Character 'X' )
             , ( "C", Character 'C' )
             , ( "V", Character 'V' )
             , ( "B", Character 'B' )
             , ( "N", Character 'N' )
             , ( "M", Character 'M' )
             , ( "DEL", Backspace )
             ]
                |> List.map (viewKeyboardLetter board)
            )
        ]


viewKeyboardLetter : Board -> ( String, Msg ) -> Html Msg
viewKeyboardLetter board ( letter, msg ) =
    let
        t =
            board
                |> boardToList
                |> flatten2D
                |> List.foldl
                    (\cell acc ->
                        case cell of
                            Empty ->
                                acc

                            Correct l ->
                                if (l |> String.fromChar |> String.toUpper) == letter then
                                    cell

                                else
                                    acc

                            Incorrect l ->
                                if (l |> String.fromChar |> String.toUpper) == letter then
                                    cell

                                else
                                    acc

                            Misplaced l ->
                                if (l |> String.fromChar |> String.toUpper) == letter then
                                    cell

                                else
                                    acc
                    )
                    Empty

        bgColor =
            case t of
                Misplaced _ ->
                    "bg-yellow-500 text-white"

                Correct _ ->
                    "bg-green-500 text-white"

                Incorrect _ ->
                    "bg-gray-500 text-white"

                Empty ->
                    "bg-white text-black"
    in
    button
        [ class (bgColor ++ " text-sm p-5 shadow-sm shadow-green-400 flex justify-center items-center rounded-xl")
        , HtmlEvents.onClick msg
        ]
        [ text letter ]
