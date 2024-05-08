namespace tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser
open Evaluator

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestStringParsing() =
        let res = parse "hello world"

        match res with
        | Some(ast) ->
            let expected = Lines([ FormattedTexts([ String "hello world" ]) ])
            Assert.AreEqual(expected, ast)
        | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestStringInterpretation() =
        let maybe_ast = parse "hello world"

        match maybe_ast with
        | Some(ast) ->
            match ast with
            | Lines(lines) ->
                let list_caps = find_list_caps ast
                let res = (evalLines lines list_caps)
                let expected = "hello world\\\\ \n"
                Assert.AreEqual(expected, res)
        | None -> Assert.IsTrue(false)
