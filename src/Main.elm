port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Page.SelectCsvSource as SelectCsvSource
import Page.ViewPrompt as ViewPrompt


port rememberCsvUrl : String -> Cmd a



---- MODEL ----


type alias Flags =
    List String


type Model
    = SelectCsvSourcePage SelectCsvSource.Model
    | ViewPromptPage ViewPrompt.Model


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( page, cmd ) =
            SelectCsvSource.init flags
    in
    ( SelectCsvSourcePage page, Cmd.map HandleSelectCsvSource cmd )


type Msg
    = HandleSelectCsvSource SelectCsvSource.Msg
    | HandleViewPrompt ViewPrompt.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( HandleSelectCsvSource subMsg, SelectCsvSourcePage page ) ->
            let
                ( ( newPage, newMsg ), prompts ) =
                    SelectCsvSource.update subMsg page
            in
            case prompts of
                Nothing ->
                    ( SelectCsvSourcePage newPage
                    , Cmd.map HandleSelectCsvSource newMsg
                    )

                Just { loadedPrompts, url } ->
                    let
                        ( newPage_, newMsg_ ) =
                            ViewPrompt.init loadedPrompts

                        (SelectCsvSource.Url url_) =
                            url
                    in
                    ( ViewPromptPage newPage_, Cmd.batch [ Cmd.map HandleViewPrompt newMsg_, rememberCsvUrl url_ ] )

        ( HandleViewPrompt subMsg, ViewPromptPage page ) ->
            let
                ( newPage, newMsg ) =
                    ViewPrompt.update subMsg page
            in
            ( ViewPromptPage newPage, Cmd.map HandleViewPrompt newMsg )

        ( HandleSelectCsvSource _, _ ) ->
            ( model, Cmd.none )

        ( HandleViewPrompt _, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ] [ viewInner model ]


viewInner : Model -> Html Msg
viewInner model =
    case model of
        SelectCsvSourcePage page ->
            Html.map HandleSelectCsvSource <| SelectCsvSource.view page

        ViewPromptPage page ->
            Html.map HandleViewPrompt <| ViewPrompt.view page



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        SelectCsvSourcePage page ->
            Sub.map HandleSelectCsvSource <| SelectCsvSource.subscriptions page

        ViewPromptPage page ->
            Sub.map HandleViewPrompt <| ViewPrompt.subscriptions page
