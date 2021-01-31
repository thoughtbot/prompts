module ParseCsv exposing (CsvError(..), ParsedPrompt, parse)

import Csv
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty)
import NonemptyExtra as Nonempty
import Parser


type CsvError
    = ParseError String
    | MissingPromptIndex
    | MissingCategoryIndex
    | MissingCategoryAndPromptIndex
    | NoPromptRows


type alias ParsedPrompt =
    { categories : List String
    , context : Maybe String
    , options : List String
    , question : String
    }


parse : String -> Result CsvError (Nonempty ParsedPrompt)
parse result =
    case Csv.parse result of
        Err e ->
            Err <| ParseError <| Parser.deadEndsToString e

        Ok { headers, records } ->
            let
                categoryIndex =
                    List.findIndex ((==) "Category") headers

                promptIndex =
                    List.findIndex ((==) "Prompt") headers

                contextIndex =
                    List.findIndex ((==) "Context") headers

                optionsIndex =
                    List.findIndex ((==) "Options") headers
            in
            case ( categoryIndex, promptIndex ) of
                ( Just cIndex, Just pIndex ) ->
                    records
                        |> List.filterMap
                            (\row ->
                                case List.getAt pIndex row of
                                    Nothing ->
                                        Nothing

                                    Just promptValue ->
                                        Just
                                            { categories = List.getAt cIndex row
                                            , prompt = promptValue
                                            , context = contextIndex |> Maybe.andThen (\v -> List.getAt v row)
                                            , options = optionsIndex |> Maybe.andThen (\v -> List.getAt v row)
                                            }
                            )
                        |> buildParsedPrompts
                        |> (\res ->
                                case res of
                                    first :: rest ->
                                        Nonempty.appendNonEmpty first rest
                                            |> Ok

                                    _ ->
                                        Err NoPromptRows
                           )

                ( Just _, _ ) ->
                    Err MissingPromptIndex

                ( _, Just _ ) ->
                    Err MissingCategoryIndex

                ( _, _ ) ->
                    Err MissingCategoryAndPromptIndex


buildParsedPrompts :
    List
        { categories : Maybe String
        , prompt : String
        , context : Maybe String
        , options : Maybe String
        }
    -> List ParsedPrompt
buildParsedPrompts results =
    let
        parseContext context =
            context
                |> Maybe.andThen
                    (\input ->
                        case String.trim input of
                            "" ->
                                Nothing

                            v ->
                                Just v
                    )

        f item acc =
            acc
                ++ [ { categories = parseList item.categories
                     , question = item.prompt
                     , context = parseContext item.context
                     , options = parseList item.options
                     }
                   ]
    in
    List.foldl f [] results


parseList : Maybe String -> List String
parseList input =
    case Maybe.map String.trim input of
        Nothing ->
            []

        Just "" ->
            []

        Just v ->
            String.split "," v
                |> List.map String.trim
