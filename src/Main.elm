module Main exposing (..)

import Browser
import Browser.Events exposing (onClick, onKeyUp)
import Html exposing (Html, button, div, h1, header, text)
import Html.Attributes exposing (class)
import Html.Events as HtmlEvents
import Json.Decode as Decode
import List.Extra as ListExtra
import Svg exposing (svg)
import Svg.Attributes as SvgAttr exposing (path)
import Time
import Utils exposing (flatten2D)
import Vector5 exposing (Vector5)
import Vector6 exposing (Vector6)
import Words exposing (dictionary)


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
    , currentWord : CurrentWord
    , correctWord : String
    , displayToast : Bool
    }


type alias Board =
    Vector6 Row


type alias Row =
    Vector5 Letter


type Letter
    = Misplaced Char
    | Correct Char
    | Incorrect Char
    | Empty


type alias CurrentWord =
    Vector5 InputChar


type InputChar
    = Filled Char
    | Unfilled


type Msg
    = Character Char
    | Control String
    | CloseToast Time.Posix


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { board = Vector6.repeat (Vector5.repeat Empty)
      , currentWord = Vector5.repeat Unfilled
      , correctWord = "cigar"
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
            Character char

        _ ->
            Control string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CloseToast _ ->
            ( { model | displayToast = False }, Cmd.none )

        Character char ->
            if char /= ' ' then
                model.currentWord
                    |> Vector5.toIndexedList
                    |> ListExtra.find (\( _, x ) -> x == Unfilled)
                    |> Maybe.map Tuple.first
                    |> Maybe.map
                        (\i ->
                            ( { model
                                | currentWord =
                                    model.currentWord
                                        |> Vector5.set i (Filled (Char.toLocaleUpper char))
                              }
                            , Cmd.none
                            )
                        )
                    |> Maybe.withDefault
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        Control control ->
            case control of
                "Backspace" ->
                    model.currentWord
                        |> Vector5.toIndexedList
                        |> List.filter (\( _, x ) -> x /= Unfilled)
                        |> ListExtra.last
                        |> Maybe.map Tuple.first
                        |> Maybe.map
                            (\i ->
                                ( { model
                                    | currentWord =
                                        model.currentWord
                                            |> Vector5.set i Unfilled
                                  }
                                , Cmd.none
                                )
                            )
                        |> Maybe.withDefault
                            ( model
                            , Cmd.none
                            )

                "Enter" ->
                    let
                        completed =
                            model.currentWord
                                |> Vector5.toList
                                |> List.all (\x -> x /= Unfilled)

                        word =
                            model.currentWord
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

                        valid =
                            List.member word dictionary
                    in
                    if completed && valid then
                        let
                            boardList =
                                model.board
                                    |> Vector6.map Vector5.toList
                                    |> Vector6.toIndexedList

                            rowIndex =
                                boardList
                                    |> ListExtra.find
                                        (\( _, row ) ->
                                            row
                                                |> List.all (\l -> l == Empty)
                                        )
                                    |> Maybe.map Tuple.first

                            newRow =
                                List.map2
                                    (\w l ->
                                        if w == l then
                                            Correct w

                                        else
                                            let
                                                exists =
                                                    List.any (\x -> x == w) (String.toList model.correctWord)
                                            in
                                            if exists then
                                                Misplaced w

                                            else
                                                Incorrect w
                                    )
                                    (String.toList word)
                                    (String.toList model.correctWord)

                            ( _, newRowVector ) =
                                Vector5.fromListWithDefault Empty newRow
                        in
                        case rowIndex of
                            Just index ->
                                ( { model
                                    | board = Vector6.set index newRowVector model.board
                                    , currentWord = Vector5.repeat Unfilled
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                --game over
                                ( model, Cmd.none )

                    else
                        ( { model | displayToast = not valid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen font-mono" ]
        [ viewHeader
        , viewBoard model
        , viewToast model.displayToast
        , viewKeyboard
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
            model.board
                |> Vector6.map Vector5.toList
                |> Vector6.toList

        previous =
            boardList
                |> ListExtra.takeWhile
                    (\row ->
                        row |> List.all (\x -> x /= Empty)
                    )
                |> flatten2D
                |> List.map viewLetter

        currentRowHtml =
            model.currentWord
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


letters : List String
letters =
    [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]


viewKeyboard : Html Msg
viewKeyboard =
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
                |> List.map viewKeyboardLetter
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
                |> List.map viewKeyboardLetter
            )
        , div [ class "grid grid-cols-9 gap-2" ]
            ([ ( "ENTER", Control "Enter" )
             , ( "Z", Character 'Z' )
             , ( "X", Character 'X' )
             , ( "C", Character 'C' )
             , ( "V", Character 'V' )
             , ( "B", Character 'B' )
             , ( "N", Character 'N' )
             , ( "M", Character 'M' )
             , ( "DEL", Control "Backspace" )
             ]
                |> List.map viewKeyboardLetter
            )
        ]


viewKeyboardLetter : ( String, Msg ) -> Html Msg
viewKeyboardLetter ( letter, msg ) =
    button
        [ class "text-sm p-5 shadow-sm shadow-green-400 flex justify-center items-center rounded-xl"
        , HtmlEvents.onClick msg
        ]
        [ text letter ]
