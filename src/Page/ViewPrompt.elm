module Page.ViewPrompt exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (class, tabindex)
import Html.Events exposing (onClick)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Keyboard
import NonemptyExtra as Nonempty
import Random
import Random.List as Random
import SelectList
import Types exposing (Context(..), Prompt, Question(..))


type alias Model =
    { categories : List Types.Category
    , prompts : SelectList.SelectList Types.Prompt
    }


type Msg
    = MoveForward
    | MoveBackward
    | ShufflePrompts Prompt (List Prompt)
    | Shuffle
    | HandleKeyboardEvent KeyboardEvent


init : Types.Prompts -> ( Model, Cmd Msg )
init prompts =
    let
        selectList =
            SelectList.fromNonempty <| Types.getPrompts prompts
    in
    ( { categories = Types.categories prompts
      , prompts = selectList
      }
    , shufflePrompts selectList
    )


moveForward : Model -> Model
moveForward model =
    { model | prompts = SelectList.moveForward model.prompts }


moveBackward : Model -> Model
moveBackward model =
    { model | prompts = SelectList.moveBackward model.prompts }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveForward ->
            ( moveForward model
            , Cmd.none
            )

        MoveBackward ->
            ( moveBackward model
            , Cmd.none
            )

        ShufflePrompts prompt prompts ->
            ( { model | prompts = SelectList.fromNonempty (Nonempty.appendNonEmpty prompt prompts) }
                |> moveForward
            , Cmd.none
            )

        Shuffle ->
            ( model, shufflePrompts model.prompts )

        HandleKeyboardEvent keyboardEvent ->
            case ( keyboardEvent.keyCode, keyboardEvent.shiftKey ) of
                ( Keyboard.Left, _ ) ->
                    ( moveBackward model, Cmd.none )

                ( Keyboard.Right, _ ) ->
                    ( moveForward model, Cmd.none )

                ( Keyboard.R, True ) ->
                    ( model, shufflePrompts model.prompts )

                _ ->
                    ( model, Cmd.none )


shufflePrompts : SelectList.SelectList Prompt -> Cmd Msg
shufflePrompts selectList =
    let
        first =
            SelectList.selected selectList

        rest =
            SelectList.before selectList ++ SelectList.after selectList
    in
    Random.generate (ShufflePrompts first) (Random.shuffle rest)


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)


view : Model -> Html Msg
view model =
    let
        prompt =
            SelectList.selected model.prompts

        context =
            case Types.getPromptContext prompt of
                Nothing ->
                    text ""

                Just (Context v) ->
                    p [ class "context" ] [ strong [] [ text "Context: " ], text v ]

        promptQuestion =
            case Types.getPromptQuestion prompt of
                Question v ->
                    [ text v ]

                QuestionWithPlaceholder before placeholder after ->
                    [ text before
                    , strong [] [ text placeholder ]
                    , text after
                    ]
    in
    div [ class "prompt" ]
        [ context
        , p [ class "main" ] promptQuestion
        , div [ class "meta" ]
            [ categoriesList <| Types.getPromptCategories prompt
            , nextPromptButton
            ]
        ]


categoriesList : List Types.Category -> Html a
categoriesList categories =
    ul [ class "categories" ] <| List.map (\c -> li [] [ text <| Types.categoryValue c ]) categories


nextPromptButton : Html Msg
nextPromptButton =
    div [ class "refresh" ]
        [ button [ onClick Shuffle, tabindex 1 ] [ i [ class "fas fa-sync" ] [] ]
        , button [ onClick MoveBackward, tabindex 2 ] [ i [ class "fas fa-step-backward" ] [] ]
        , button [ onClick MoveForward, tabindex 0 ] [ i [ class "fas fa-step-forward" ] [] ]
        ]
