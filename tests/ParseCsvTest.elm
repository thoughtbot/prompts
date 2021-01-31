module ParseCsvTest exposing (all)

import Expect
import NonemptyExtra
import ParseCsv
import Test exposing (..)


all : Test
all =
    describe "Parsing"
        [ test "works when at least one row is provided" <|
            \_ ->
                Expect.equal
                    (Ok
                        (NonemptyExtra.appendNonEmpty
                            { categories =
                                [ "work" ]
                            , context = Nothing
                            , options = []
                            , question = "What are you working on?"
                            }
                            []
                        )
                    )
                    (ParseCsv.parse "Category,Prompt\nwork,What are you working on?")
        , test "errors when no prompts are provided" <|
            \_ ->
                Expect.equal
                    (Err ParseCsv.NoPromptRows)
                    (ParseCsv.parse "Category,Prompt")
        , test "errors without a Prompt column" <|
            \_ ->
                Expect.equal
                    (Err ParseCsv.MissingPromptIndex)
                    (ParseCsv.parse "Category")
        , test "errors without a Category column" <|
            \_ ->
                Expect.equal
                    (Err ParseCsv.MissingCategoryIndex)
                    (ParseCsv.parse "Prompt")
        , test "errors without a Category and Prompt column" <|
            \_ ->
                Expect.equal
                    (Err ParseCsv.MissingCategoryAndPromptIndex)
                    (ParseCsv.parse "Context,Options")
        , test "works with richer data" <|
            \_ ->
                Expect.equal
                    (Ok
                        (NonemptyExtra.appendNonEmpty
                            { categories =
                                [ "work", "fun" ]
                            , context = Just "Imagine coming out of a fun meeting."
                            , options = [ "pre-COVID ice cream parties", "working on a project" ]
                            , question = "Did you talk about {}?"
                            }
                            []
                        )
                    )
                    (ParseCsv.parse "Category,Context,Options,Prompt\n\"work,fun\",Imagine coming out of a fun meeting.,\"pre-COVID ice cream parties,working on a project\",Did you talk about {}?")
        ]
