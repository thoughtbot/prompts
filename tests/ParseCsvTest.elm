module ParseCsvTest exposing (all)

import Expect
import List.Nonempty as Nonempty
import ParseCsv
import Test exposing (..)


all : Test
all =
    describe "Parsing"
        [ test "works when at least one row is provided" <|
            \_ ->
                Expect.equal
                    (Ok
                        (Nonempty.fromElement
                            { categories = []
                            , context = Nothing
                            , options = []
                            , question = "What are you working on?"
                            }
                        )
                    )
                    (ParseCsv.parse "Prompt\nWhat are you working on?")
        , test "errors when the prompt is empty" <|
            \_ ->
                Expect.equal
                    (Err <| ParseCsv.DecodeError "empty string")
                    (ParseCsv.parse "Prompt\n ")
        , test "errors when no prompts are provided" <|
            \_ ->
                Expect.equal
                    (Err ParseCsv.NoPromptRows)
                    (ParseCsv.parse "Prompt")
        , test "errors without a Prompt column" <|
            \_ ->
                Expect.equal
                    (Err ParseCsv.MissingPromptIndex)
                    (ParseCsv.parse "Category\nwork")
        , test "works with richer data" <|
            \_ ->
                Expect.equal
                    (Ok
                        (Nonempty.fromElement
                            { categories =
                                [ "work", "fun" ]
                            , context = Just "Imagine coming out of a fun meeting."
                            , options = [ "pre-COVID ice cream parties", "working on a project" ]
                            , question = "Did you talk about {}?"
                            }
                        )
                    )
                    (ParseCsv.parse "Category,Context,Options,Prompt\n\"work,fun\",Imagine coming out of a fun meeting.,\"pre-COVID ice cream parties,working on a project\",Did you talk about {}?")
        ]
