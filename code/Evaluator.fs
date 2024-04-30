module Evaluator

open Parser

(* 
    Record whether a line is the start/end of a list, 
    and whether the line is an item (for spacing purposes).
    We know a Start is always an item, and an end never is
*)
type ListCap =
    | Start
    | End
    | Nothing of bool

let doc_preamble =
    [ "\\documentclass[10pt]{article}"
      "\\usepackage{geometry}"
      "\\geometry{top=1in, bottom=1in, left=.5in, right=.5in}"
      "\\geometry{paperwidth=8.5in, paperheight=11in}"
      "\\setlength{\\parindent}{0pt}"
      "\\begin{document}" ]
    |> List.fold (fun acc s -> acc + s + "\n") ""

let doc_ending = [ "\\end{document}" ] |> List.fold (fun acc s -> acc + s + "\n") ""

let find_list_caps (ast: Expression) : ListCap list =
    let rec find_list_caps' (lines: Line list) (previousItem: bool) =
        match lines with
        | line :: ls' ->
            match line with
            | FormattedTexts(fs) ->
                match fs with
                | f_expr :: _ ->
                    match f_expr with
                    | Modifier(m, _) when m = "ITEM" && (not previousItem) -> Start :: (find_list_caps' ls' true)
                    | Modifier(m, _) when m = "ITEM" -> Nothing(true) :: find_list_caps' ls' true
                    | Modifier(_) when previousItem -> End :: find_list_caps' ls' false
                    | Modifier(_) -> Nothing(false) :: find_list_caps' ls' false
                    | String(_) when previousItem -> End :: find_list_caps' ls' false
                    | String(_) -> Nothing(false) :: find_list_caps' ls' false
                | [] -> Nothing(false) :: find_list_caps' ls' false
        | [] when previousItem -> [ End ]
        | [] -> []

    match ast with
    | Lines(lines) -> find_list_caps' lines false


let rec evalModifierCommand (command: string) (inner: string) =
    match command with
    | "BOLD" -> sprintf "\\textbf{%s}" inner
    | "UNDERLINE" -> sprintf "\\underline{%s}" inner
    | "ITEM" -> sprintf "\\item %s" inner // TODO: shrink bullet point, indent
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

let rec evalLines (lines: Line list) (caps: ListCap list) : string =
    match lines, caps with
    | FormattedTexts(line) :: ls', Start :: caps' ->
        "\\begin{itemize}\n" + (evalFormattedTexts line) + "\n" + (evalLines ls' caps')
    | FormattedTexts(line) :: ls', End :: caps' ->
        "\\end{itemize}\n"
        + (evalFormattedTexts line)
        + "\\\\ \n"
        + (evalLines ls' caps')
    | FormattedTexts(line) :: ls', Nothing(isItem) :: caps' ->
        (evalFormattedTexts line)
        + (if isItem then "\n" else "\\\\ \n")
        + (evalLines ls' caps')
    | [], _ -> "\\end{itemize}\n"
    | _ -> ""

let eval (ast: Expression) (fileName: string) : unit =
    printfn ""

    let list_caps = find_list_caps ast
    printfn "%A" list_caps

    match ast with
    | Lines(lines) ->
        let res = doc_preamble + (evalLines lines list_caps) + doc_ending
        System.IO.File.WriteAllText(fileName.Replace(".txt", ".tex"), res)
