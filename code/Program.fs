open System.IO
open Parser
open Evaluator

[<EntryPoint>]
let main args =
    if args.Length <> 1 || not (File.Exists(args[0])) then
        printfn "Usage: dotnet run filename.txt"
        exit 1

    if not (args[0].EndsWith ".txt") then
        printfn "Input file must be a text file"
        exit 1

    let lines = System.IO.File.ReadAllText args[0]
    let ast_maybe = parse lines

    match ast_maybe with
    | Some ast ->
        printfn "%A" ast
        eval ast args[0]
    | None -> exit 1

    0
