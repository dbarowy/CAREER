namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Parser

[<TestClass>]
type TestClass() =

    [<TestMethod>]
    member this.TestMethodPassing() =
        let res = parse "hello world"

        match res with
        | Some(ast) ->
            let expected = Lines([ FormattedTexts([ String "hello world" ]) ])
            Assert.AreEqual(expected, ast)
        | None -> Assert.IsTrue(false)
