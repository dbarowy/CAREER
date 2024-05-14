namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open Evaluator

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestStringParsing() =
        // lots of directory work to find the location of the test input
        let c_dir = System.Environment.CurrentDirectory
        let split_dir = c_dir.Split [| '/' |]
        let test_dir_index = Array.findIndex (fun dir -> dir = "tests") split_dir
        let split_test_dir = Array.take (test_dir_index + 1) split_dir
        let test_dir = (String.concat "/" split_test_dir) + "/test_input.txt"

        let lines = System.IO.File.ReadAllText test_dir
        let maybe_ast = parse lines

        match maybe_ast with
        | Some(ast) ->
            let expected = Lines([ FormattedTexts([ String "hello world" ]) ])
            Assert.AreEqual(expected, ast)
        | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestStringEvaluation() =
        // lots of directory work to find the location of the test input
        let c_dir = System.Environment.CurrentDirectory
        let split_dir = c_dir.Split [| '/' |]
        let test_dir_index = Array.findIndex (fun dir -> dir = "tests") split_dir
        let split_test_dir = Array.take (test_dir_index + 1) split_dir
        let test_dir = (String.concat "/" split_test_dir) + "/test_input.txt"

        let lines = System.IO.File.ReadAllText test_dir
        let maybe_ast = parse lines

        match maybe_ast with
        | Some(ast) ->
            match ast with
            | Lines(lines) ->
                (*
                    this is the closest to end to end testing I can get without directly checking a pdf or latex code.
                    checking latex code against my own latex code feels a bit silly, as the test will have to change 
                    any time I add any new preamble or feature to the latex. Checkign the raw string output feels 
                    closer to a true check of functionality.
                *)


                let list_caps = find_list_caps ast
                let res = (evalLines lines list_caps)
                let expected = "hello world\\\\ \n"
                Assert.AreEqual(expected, res)
        | None -> Assert.IsTrue(false)
