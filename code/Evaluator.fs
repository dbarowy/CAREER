module Evaluator

open Parser

let doc_preamble =
    [ "\\documentclass[10pt]{article}"
      "\\setlength{\\parindent}{0pt}"
      "\\begin{document}" ]
    |> List.fold (fun acc s -> acc + s + "\n") ""

let doc_ending = [ "\\end{document}" ] |> List.fold (fun acc s -> acc + s + "\n") ""

let rec evalModifierCommand (command: string) (inner: string) =
    match command with
    | "BOLD" -> sprintf "\\textbf{%s}" inner
    | "UNDERLINE" -> sprintf "\\underline{%s}" inner
    | "ITEM" -> sprintf "\\textbullet %s" inner // TODO: shrink bullet point, indent
    | _ -> inner // TODO: add rest of commands, error out here when no commands are matches

let rec evalFormattedTexts (formattedTexts: FormattedText list) =
    let rec evalFormattedText (f: FormattedText) =
        match f with
        | Modifier(s, f) ->
            let res = evalFormattedTexts f
            evalModifierCommand s res
        | String(s) -> s

    match formattedTexts with
    | f :: fs' -> evalFormattedText (f) + evalFormattedTexts (fs')
    | [] -> ""

let rec evalLines (lines: Line list) : string =
    match lines with
    | FormattedTexts(line) :: lines' -> evalFormattedTexts line + "\\\\ \n" + evalLines (lines')
    | [] -> ""

let eval (ast: Expression) (fileName: string) : string =
    printfn ""

    match ast with
    | Lines(lines) ->
        let res = doc_preamble + (evalLines lines) + doc_ending
        System.IO.File.WriteAllText(fileName.Replace(".txt", ".tex"), res)
        res
