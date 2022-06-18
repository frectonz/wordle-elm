module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, header, text)
import Html.Attributes exposing (class)
import Keyboard exposing (Key(..), RawKey)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = KeyUp RawKey


type alias Model =
    { gameState : List String
    , gamePosition : Int
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { gameState = List.repeat 30 " "
      , gamePosition = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.ups KeyUp
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newChar =
            case msg of
                KeyUp key ->
                    Keyboard.anyKeyOriginal key
                        |> Maybe.andThen
                            (\k ->
                                case k of
                                    Character char ->
                                        if isNumber char then
                                            Nothing

                                        else
                                            Just char

                                    _ ->
                                        Nothing
                            )
    in
    case newChar of
        Just char ->
            let
                left =
                    List.take model.gamePosition model.gameState

                right =
                    List.drop (model.gamePosition + 1) model.gameState

                newGameState =
                    left ++ (char :: right)
            in
            ( { model
                | gameState = newGameState
                , gamePosition = model.gamePosition + 1
              }
            , Cmd.none
            )

        Nothing ->
            ( model
            , Cmd.none
            )


isNumber : String -> Bool
isNumber str =
    case str of
        "0" ->
            True

        "1" ->
            True

        "2" ->
            True

        "3" ->
            True

        "4" ->
            True

        "5" ->
            True

        "6" ->
            True

        "7" ->
            True

        "8" ->
            True

        "9" ->
            True

        _ ->
            False


view : Model -> Html msg
view model =
    div [ class "w-screen h-screen font-mono" ]
        [ header [ class "border-b border-black" ]
            [ h1 [ class "text-3xl text-center py-4 font-bold" ] [ text "WORDLE" ]
            ]
        , div
            [ class "grid grid-cols-5 grid-rows-6 my-10 mx-auto gap-4 w-[500px] h-[500px]"
            ]
            (model.gameState
                |> List.map
                    (\str ->
                        div
                            [ class "text-4xl shadow-sm shadow-blue-400 flex justify-center items-center"
                            ]
                            [ text str ]
                    )
            )
        ]
