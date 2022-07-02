port module Main exposing (..)

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
import Html exposing (Html, div, h1, header, p, span, strong, text)
import Html.Attributes exposing (class, id)
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



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port vibrate : () -> Cmd msg


port fadeStartModalOut : () -> Cmd msg



-- MODEL


type alias Model =
    { board : Board
    , inputWord : InputWord
    , secretWord : String
    , displayToast : Bool
    , gameStarted : Bool
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { board = initalBoard
      , inputWord = initialInputWord
      , secretWord = "cigar"
      , displayToast = False
      , gameStarted = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Character Char
    | CloseToast
    | Backspace
    | SubmitInputWord
    | NoOp
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model
                | gameStarted = True
              }
            , fadeStartModalOut ()
            )

        NoOp ->
            ( model, Cmd.none )

        CloseToast ->
            ( { model | displayToast = False }, Cmd.none )

        Character char ->
            let
                gameCompleted : Bool
                gameCompleted =
                    findFirstEmptyRow model.board == Nothing
            in
            if not gameCompleted && model.gameStarted then
                ( { model
                    | inputWord = addCharToInputWord char model.inputWord
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

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
                let
                    newDisplayToast : Bool
                    newDisplayToast =
                        isInputWordCompleted model.inputWord
                            && not (isInputWordValid model.inputWord)
                in
                ( { model
                    | displayToast = newDisplayToast
                  }
                , if newDisplayToast then
                    vibrate ()

                  else
                    Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyUp keyDecoder
        , if model.displayToast then
            Time.every 2000 (\_ -> CloseToast)

          else
            Sub.none
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



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "font-mono" ]
        [ div [ class "w-screen h-screen flex flex-col items-start justify-between" ]
            [ viewHeader
            , viewBoard model
            , viewKeyboard model.board
            ]
        , viewToast model.displayToast
        , viewStartModal
        ]


viewHeader : Html Msg
viewHeader =
    header [ class "w-screen border-b border-green-500" ]
        [ h1 [ class "text-4xl text-center py-4 font-bold text-green-500 text-shadow" ] [ text "WORDLE" ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        boardList : List (List Letter)
        boardList =
            boardToList model.board

        previous : List (Html Msg)
        previous =
            boardList
                |> ListExtra.takeWhile
                    (List.all (\x -> x /= Empty))
                |> flatten2D
                |> List.map viewLetter

        currentRowHtml : List (Html Msg)
        currentRowHtml =
            if List.length previous == 30 then
                []

            else
                model.inputWord
                    |> Vector5.toIndexedList
                    |> List.map
                        (\( index, input ) ->
                            let
                                animationClass : String
                                animationClass =
                                    Vector5.nextIndex index
                                        |> Maybe.map (\nextIndex -> Vector5.get nextIndex model.inputWord == Unfilled && input /= Unfilled)
                                        |> Maybe.withDefault (input /= Unfilled)
                                        |> (\isLastInput ->
                                                if isLastInput then
                                                    "animate-pop"

                                                else
                                                    ""
                                           )
                            in
                            div [ class ("bg-white text-2xl border border-green-500 flex justify-center items-center " ++ animationClass) ]
                                [ text
                                    (case input of
                                        Filled c ->
                                            String.fromChar c

                                        Unfilled ->
                                            " "
                                    )
                                ]
                        )

        next : List (Html Msg)
        next =
            boardList
                |> ListExtra.takeWhileRight
                    (\row ->
                        row |> List.all (\x -> x == Empty)
                    )
                |> List.indexedMap
                    (\index row ->
                        if index == 0 then
                            []

                        else
                            row
                    )
                |> flatten2D
                |> List.map viewLetter
    in
    div
        [ class "grid grid-cols-5 grid-rows-6 my-10 mx-auto gap-4 p-2 w-[320px] h-[320px] sm:w-[400px] sm:h-[400px]"
        ]
        (previous ++ currentRowHtml ++ next)


viewLetter : Letter -> Html Msg
viewLetter letter =
    let
        bgColor : String
        bgColor =
            case letter of
                Misplaced _ ->
                    "bg-yellow-500 text-white"

                Correct _ ->
                    "bg-green-500 text-white"

                Incorrect _ ->
                    "bg-gray-500 text-white"

                Empty ->
                    "bg-white border"
    in
    div
        [ class
            ("text-2xl flex justify-center items-center " ++ bgColor)
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


viewKeyboard : Board -> Html Msg
viewKeyboard board =
    div [ class "md:w-[600px] p-2 mx-auto mb-6 grid grid-rows-3 gap-3" ]
        [ div [ class "grid grid-cols-10 sm:gap-2 gap-1" ]
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
        , div [ class "grid grid-cols-9 gap-2 sm:px-4 px-2" ]
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
        , div [ class "grid grid-cols-11 gap-2" ]
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
        letterStatus : Letter
        letterStatus =
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

        bgColor : String
        bgColor =
            case letterStatus of
                Misplaced _ ->
                    "bg-yellow-500 text-white"

                Correct _ ->
                    "bg-green-500 text-white"

                Incorrect _ ->
                    "bg-gray-500 text-white"

                Empty ->
                    "bg-white text-black border"

        letterSize : String
        letterSize =
            if String.length letter == 1 then
                "col-span-1"

            else
                "col-span-2"
    in
    div
        [ class ("sm:text-sm text-xs md:p-5 p-3 flex justify-center items-center pop-on-active active:scale-75 " ++ bgColor ++ " " ++ letterSize)
        , HtmlEvents.onClick msg
        ]
        [ text letter ]


viewToast : Bool -> Html Msg
viewToast displayToast =
    let
        visible : String
        visible =
            if displayToast then
                "visible"

            else
                "hidden"
    in
    div
        [ class
            ("fixed top-[55vh] left-1/2 -translate-x-1/2 bg-gray-500 py-2 px-4 text-white rounded-lg shadow-lg w-fit transition-all " ++ visible)
        ]
        [ text "NOT IN WORD LIST" ]


viewStartModal : Html Msg
viewStartModal =
    div []
        [ div [ id "backdrop" ] []
        , div
            [ id "modal"
            , class "fixed top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 bg-white shadow-2xl py-8 px-4 w-[95vw] sm:w-[500px] md:w-[600px] text-sm sm:text-base"
            ]
            [ p [ class "mb-2" ]
                [ span []
                    [ text "Guess the "
                    ]
                , strong [] [ text "WORDLE" ]
                , span [] [ text " in six tries." ]
                ]
            , p [ class "mb-2" ]
                [ text "Each guess must be a valid five-letter word. Hit the enter button to submit." ]
            , h1 [ class "my-4 pt-2 border-t" ] [ text "Examples" ]
            , div [ class "grid grid-cols-5 gap-2 h-10 mb-2 w-[200px]" ]
                [ viewLetter (Correct 'W')
                , viewLetter (Incorrect 'E')
                , viewLetter (Incorrect 'A')
                , viewLetter (Incorrect 'R')
                , viewLetter (Incorrect 'Y')
                ]
            , p [ class "mb-2" ]
                [ span [] [ text "The letter " ]
                , strong [] [ text "W" ]
                , span [] [ text " is in the word and in the correct spot." ]
                ]
            , div [ class "grid grid-cols-5 gap-2 h-10 mb-2 w-[200px]" ]
                [ viewLetter (Incorrect 'P')
                , viewLetter (Misplaced 'I')
                , viewLetter (Incorrect 'L')
                , viewLetter (Incorrect 'L')
                , viewLetter (Incorrect 'S')
                ]
            , p [ class "mb-2" ]
                [ span [] [ text "The letter " ]
                , strong [] [ text "I" ]
                , span [] [ text " is in the word but in the wrong spot." ]
                ]
            , div [ class "grid grid-cols-5 gap-2 h-10 mb-2 w-[200px]" ]
                [ viewLetter (Incorrect 'V')
                , viewLetter (Incorrect 'A')
                , viewLetter (Incorrect 'G')
                , viewLetter (Incorrect 'U')
                , viewLetter (Incorrect 'E')
                ]
            , p [ class "mb-2 pb-2 border-b" ]
                [ text "All of the letters are not in the word." ]
            , div
                [ class "px-4 py-2 bg-green-500 text-white rounded-sm text-center cursor-pointer active:scale-90"
                , HtmlEvents.onClick StartGame
                ]
                [ text "START GAME" ]
            ]
        ]
