module ParseCsv exposing (CsvError(..), ParsedPrompt, parse)

import Csv.Decode as Decode exposing (Decoder)
import List.Nonempty as Nonempty exposing (Nonempty)
import NonemptyExtra as Nonempty


type CsvError
    = ParseError String
    | DecodeError String
    | MissingPromptIndex
    | NoPromptRows


type alias ParsedPrompt =
    { categories : List String
    , context : Maybe String
    , options : List String
    , question : String
    }


parse : String -> Result CsvError (Nonempty ParsedPrompt)
parse result =
    Decode.decodeCsv
        Decode.FieldNamesFromFirstRow
        parsedPromptDecoder
        result
        |> Result.mapError decodeError
        |> Result.andThen (Nonempty.fromList >> Result.fromMaybe NoPromptRows)


decodeError : Decode.Error -> CsvError
decodeError error =
    case error of
        Decode.ParsingError problem ->
            ParseError "parsing failed"

        Decode.DecodingError { row, problem } ->
            case problem of
                Decode.ExpectedField "Prompt" ->
                    MissingPromptIndex

                Decode.Failure v ->
                    DecodeError v

                _ ->
                    DecodeError "decoding failed"


nonEmptyString : Decode.Decoder String
nonEmptyString =
    Decode.string
        |> Decode.andThen
            (\v ->
                case String.trim v of
                    "" ->
                        Decode.fail "empty string"

                    _ ->
                        Decode.succeed v
            )


parsedPromptDecoder : Decode.Decoder ParsedPrompt
parsedPromptDecoder =
    let
        optionalString =
            Decode.blank Decode.string

        decodeCategory =
            Decode.oneOf
                (Decode.field "Category" (Decode.map parseList optionalString))
                [ Decode.succeed [] ]

        decodeContext =
            Decode.oneOf
                (Decode.field "Context" optionalString)
                [ Decode.succeed Nothing ]

        decodeOptions =
            Decode.oneOf
                (Decode.field "Options" (Decode.map parseList optionalString))
                [ Decode.succeed [] ]

        decodePrompt =
            Decode.field "Prompt" nonEmptyString
    in
    Decode.into ParsedPrompt
        |> Decode.pipeline decodeCategory
        |> Decode.pipeline decodeContext
        |> Decode.pipeline decodeOptions
        |> Decode.pipeline decodePrompt


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
