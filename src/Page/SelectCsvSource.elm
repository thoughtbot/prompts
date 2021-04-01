module Page.SelectCsvSource exposing
    ( Model
    , Msg
    , Url(..)
    , init
    , subscriptions
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (class, for, id, required, tabindex, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import ParseCsv
import RemoteData
import String.Extra as String
import Types exposing (ApplicationError(..))


type Url
    = Url String


type PreviousUrls
    = PreviousUrls (List Url)


type Model
    = NoDocumentSelected PreviousUrls
    | DocumentSelected PreviousUrls Url
    | DocumentLoading PreviousUrls Url
    | DocumentLoadedSuccessfully PreviousUrls Url Types.Prompts
    | DocumentFailedParsing PreviousUrls Url ApplicationError
    | DocumentFailed PreviousUrls Url Http.Error


type Msg
    = SetUrl String
    | SubmitForm Url
    | GotResponse Url (RemoteData.WebData String)
    | Proceed


type alias Payload =
    { loadedPrompts : Types.Prompts
    , url : Url
    }


init : List String -> ( Model, Cmd Msg )
init previousUrls_ =
    ( NoDocumentSelected (PreviousUrls <| List.map Url previousUrls_), Cmd.none )


previousUrls : Model -> PreviousUrls
previousUrls model =
    case model of
        NoDocumentSelected v ->
            v

        DocumentSelected v _ ->
            v

        DocumentLoading v _ ->
            v

        DocumentLoadedSuccessfully v _ _ ->
            v

        DocumentFailedParsing v _ _ ->
            v

        DocumentFailed v _ _ ->
            v


noPrompts : ( Model, Cmd Msg ) -> ( ( Model, Cmd Msg ), Maybe Payload )
noPrompts a =
    ( a, Nothing )


withPrompts : Types.Prompts -> Url -> ( Model, Cmd Msg ) -> ( ( Model, Cmd Msg ), Maybe Payload )
withPrompts p url a =
    ( a, Just { loadedPrompts = p, url = url } )


update : Msg -> Model -> ( ( Model, Cmd Msg ), Maybe Payload )
update msg model =
    let
        prevUrls =
            previousUrls model
    in
    case msg of
        SetUrl input ->
            case String.trim input of
                "" ->
                    ( NoDocumentSelected prevUrls, Cmd.none )
                        |> noPrompts

                otherwise ->
                    ( DocumentSelected prevUrls (Url otherwise), Cmd.none )
                        |> noPrompts

        SubmitForm url ->
            let
                (Url url_) =
                    url
            in
            ( DocumentLoading prevUrls url
            , Http.request
                { method = "get"
                , headers = [ Http.header "x-requested-with" "prompts" ]
                , url = url_
                , body = Http.emptyBody
                , expect = Http.expectString (RemoteData.fromResult >> GotResponse (Url url_))
                , timeout = Nothing
                , tracker = Nothing
                }
            )
                |> noPrompts

        GotResponse url (RemoteData.Failure error) ->
            ( DocumentFailed prevUrls url error, Cmd.none )
                |> noPrompts

        GotResponse url (RemoteData.Success value) ->
            let
                parseOutcome =
                    ParseCsv.parse value |> Result.mapError CsvError |> Result.andThen Types.buildPrompts
            in
            case parseOutcome of
                Ok prompts ->
                    ( DocumentLoadedSuccessfully prevUrls url prompts, Cmd.none )
                        |> noPrompts

                Err appError ->
                    ( DocumentFailedParsing prevUrls url appError, Cmd.none )
                        |> noPrompts

        GotResponse _ _ ->
            ( model, Cmd.none )
                |> noPrompts

        Proceed ->
            case model of
                DocumentLoadedSuccessfully _ url prompts ->
                    ( model, Cmd.none )
                        |> withPrompts prompts url

                _ ->
                    ( model, Cmd.none )
                        |> noPrompts


view : Model -> Html Msg
view model =
    case model of
        NoDocumentSelected previousUrls_ ->
            promptUrlForm previousUrls_ (Url "")

        DocumentSelected previousUrls_ url ->
            promptUrlForm previousUrls_ url

        DocumentLoading _ _ ->
            div []
                [ h2 [ class "text-center text-5xl" ] [ text "Loading..." ]
                ]

        DocumentLoadedSuccessfully _ _ prompts ->
            successfulPromptsOutcome prompts

        DocumentFailedParsing previousUrls_ url _ ->
            div []
                [ p [] [ text "Unable to parse CSV" ]
                , promptUrlForm previousUrls_ url
                ]

        DocumentFailed previousUrls_ url _ ->
            div []
                [ p [] [ text "Unable to load URL" ]
                , promptUrlForm previousUrls_ url
                ]


promptUrlForm : PreviousUrls -> Url -> Html Msg
promptUrlForm urls (Url urlValue) =
    let
        submitBehavior =
            SubmitForm (Url urlValue)
    in
    div [ class "flex flex-col space-y-8" ]
        [ h2 [ class "text-5xl font-bold lg:text-7xl" ] [ text "Welcome to Prompts!" ]
        , p [ class "text-3xl lg:text-4xl" ] [ text "Prompts is a small tool to help initiate conversations." ]
        , form [ class "flex flex-col space-y-2", onSubmit submitBehavior ]
            [ label [ for "url" ] [ text "What's the URL of the CSV you'll be using?" ]
            , div [ class "flex flex-col space-y-2 md:space-y-0 md:flex-row md:space-x-2" ]
                [ input [ class "md:w-3/4 p-2 border border-gray-300 rounded-md text-gray-800", required True, type_ "url", id "url", onInput SetUrl, value urlValue ] []
                , input [ class "md:w-1/4 p-2", buttonStyles, type_ "submit", value "Submit" ] []
                ]
            ]
        , previousUrlsList urls
        ]


buttonStyles : Attribute a
buttonStyles =
    class "bg-red-500 text-white text-opacity-90 shadow-md font-bold rounded-md cursor-pointer font-sans"


previousUrlsList : PreviousUrls -> Html Msg
previousUrlsList (PreviousUrls urls) =
    if List.length urls > 0 then
        section []
            [ h3 [] [ text "Or, view previous CSVs you've loaded" ]
            , ul [ class "" ]
                (List.map
                    (\url_ ->
                        let
                            (Url val) =
                                url_
                        in
                        li [ onClick <| SubmitForm url_ ] [ text val ]
                    )
                    urls
                )
            ]

    else
        text ""


successfulPromptsOutcome : Types.Prompts -> Html Msg
successfulPromptsOutcome prompts =
    let
        promptsCount =
            String.pluralize "prompt" "prompts" (Types.promptsList prompts |> List.length)

        categoriesCount =
            String.pluralize "category" "categories" (Types.categories prompts |> List.length)
    in
    div [ class "flex flex-col space-y-8" ]
        [ h2 [ class "text-5xl" ] [ text "Now we're cooking with gas! 🔥🔥🔥" ]
        , p []
            [ text <|
                "We were able to load "
                    ++ promptsCount
                    ++ " across "
                    ++ categoriesCount
                    ++ "."
            ]
        , button [ buttonStyles, class "px-8 py-4 self-center", tabindex 0, onClick Proceed ] [ text "Let's get going!" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
