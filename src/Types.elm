module Types exposing
    ( ApplicationError(..)
    , Category
    , Context(..)
    , Prompt
    , Prompts
    , Question(..)
    , buildPrompts
    , categories
    , categoryValue
    , getPromptCategories
    , getPromptContext
    , getPromptQuestion
    , getPrompts
    , promptsList
    , successfullyParse
    )

import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import NonemptyExtra as Nonempty
import ParseCsv exposing (ParsedPrompt)


type ApplicationError
    = CsvError ParseCsv.CsvError
    | NoPrompts


getPrompts : Prompts -> Nonempty Prompt
getPrompts prompts =
    case prompts of
        SuccessfulParse _ v ->
            v

        PartiallySuccessfulParse _ _ v ->
            v


categories : Prompts -> List Category
categories prompts =
    case prompts of
        SuccessfulParse v _ ->
            v

        PartiallySuccessfulParse v _ _ ->
            v


promptsList : Prompts -> List Prompt
promptsList =
    getPrompts >> Nonempty.toList


type Prompts
    = SuccessfulParse (List Category) (Nonempty Prompt)
    | PartiallySuccessfulParse (List Category) (List (List ExpansionError)) (Nonempty Prompt)


type ExpansionError
    = ExpectingPlaceholder Question
    | ExpectingOptions Question
    | TooManyPlaceholders Question


type Category
    = Category String


categoryValue : Category -> String
categoryValue (Category c) =
    c


type Context
    = Context String


type Question
    = Question String
    | QuestionWithPlaceholder String String String


type Prompt
    = PromptWithContext Question (List Category) Context
    | PromptWithoutContext Question (List Category)


getPromptCategories : Prompt -> List Category
getPromptCategories prompt =
    case prompt of
        PromptWithoutContext _ v ->
            v

        PromptWithContext _ v _ ->
            v


getPromptQuestion : Prompt -> Question
getPromptQuestion prompt =
    case prompt of
        PromptWithoutContext v _ ->
            v

        PromptWithContext v _ _ ->
            v


getPromptContext : Prompt -> Maybe Context
getPromptContext prompt =
    case prompt of
        PromptWithoutContext _ _ ->
            Nothing

        PromptWithContext _ _ v ->
            Just v


buildPrompt : Question -> List Category -> Maybe String -> Prompt
buildPrompt question categories_ optionalContext =
    case optionalContext of
        Nothing ->
            PromptWithoutContext question categories_

        Just context ->
            PromptWithContext question categories_ (Context context)


expandPrompt : ParsedPrompt -> Result (List ExpansionError) (List Prompt)
expandPrompt prompt =
    let
        question =
            Question prompt.question
    in
    case ( String.split "{}" prompt.question, List.length prompt.options ) of
        ( [ _ ], 0 ) ->
            Ok <| [ buildPrompt question (List.map Category prompt.categories) prompt.context ]

        ( [ _ ], _ ) ->
            Err [ ExpectingPlaceholder question ]

        ( [ _, _ ], 0 ) ->
            Err [ ExpectingOptions question ]

        ( [ first, last ], _ ) ->
            Ok <|
                List.map
                    (\option ->
                        buildPrompt (QuestionWithPlaceholder first option last) (List.map Category prompt.categories) prompt.context
                    )
                    prompt.options

        ( _, 0 ) ->
            Err [ TooManyPlaceholders question, ExpectingOptions question ]

        ( _, _ ) ->
            Err [ TooManyPlaceholders question ]


successfullyParse : Prompt -> List Prompt -> Prompts
successfullyParse first rest =
    SuccessfulParse (buildCategories <| first :: rest) (Nonempty.appendNonEmpty first rest)


buildPrompts : Nonempty ParsedPrompt -> Result ApplicationError Prompts
buildPrompts promptList =
    Nonempty.map expandPrompt promptList
        |> processResults


processResults : Nonempty (Result (List ExpansionError) (List Prompt)) -> Result ApplicationError Prompts
processResults results =
    case ( List.concat <| oks <| Nonempty.toList results, errs <| Nonempty.toList results ) of
        ( [], _ ) ->
            Err NoPrompts

        ( first :: rest, [] ) ->
            Ok <| successfullyParse first rest

        ( first :: rest, errors ) ->
            Ok <| PartiallySuccessfulParse (buildCategories <| first :: rest) errors (Nonempty.appendNonEmpty first rest)


buildCategories : List Prompt -> List Category
buildCategories xs =
    List.concatMap getPromptCategories xs
        |> List.uniqueBy categoryValue


oks : List (Result e a) -> List a
oks =
    List.filterMap Result.toMaybe


errs : List (Result e a) -> List e
errs =
    List.filterMap
        (\v ->
            case v of
                Ok _ ->
                    Nothing

                Err e ->
                    Just e
        )
