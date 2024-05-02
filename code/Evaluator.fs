module Evaluator

open Parser
open Shell
open System.IO

(* 
    Record whether a line is the start/end of a list, 
    and whether the line is an item (for spacing purposes).
    We know a Start is always an item, and an end never is
*)
type ModuleCap =
    | Start
    | End
    | Nothing of bool

(* Intro to output latex file *)
let doc_preamble =
    [ "\\documentclass[10pt]{article}"
      "\\usepackage{enumitem}"
      "\\usepackage{geometry}"
      "\\geometry{top=1in, bottom=1in, left=.5in, right=.5in}"
      "\\geometry{paperwidth=8.5in, paperheight=11in}"
      "\\setlength{\\parindent}{0pt}"
      "\\begin{document}" ]
    |> List.fold (fun acc s -> acc + s + "\n") ""

(* Ending to output latex file*)
let doc_ending = [ "\\end{document}" ] |> List.fold (fun acc s -> acc + s + "\n") ""

(* 
    Latex lists are started/ended with specific commands.
    Asking users to insert start/end list commands would be cumbersome,
    so we instead traverse the AST and record which lines constitute the
    start/end of lists.
 *)
let find_list_caps (ast: Expression) : ModuleCap list =
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

let find_title_caps (ast: Expression) : ModuleCap list =
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

(* map modifier command to associated latex code *)
let rec evalModifierCommand (command: string) (inner: string) =
    match command with
    | "HEADER" ->
        "\\vspace{-\\baselineskip}\n"
        + "\\begin{center}\n"
        + sprintf "\\Large %s\n" inner
        + "\\end{center}\n"
        + "\\vspace{-6.5ex}~"
    | "SUB_HEADER" ->
        "\\vspace{-\\baselineskip}\n"
        + "\\begin{center}\n"
        + sprintf "\\normalsize %s\n" inner
        + "\\end{center}\n"
        + "\\vspace{-6.5ex}~"
    | "ITEM" -> sprintf "\\item %s" inner
    | "BOLD" -> sprintf "\\textbf{%s}" inner
    | "UNDERLINE" -> sprintf "\\underline{%s}" inner
    | "SECTION" -> sprintf "\\textbf{%s}\\\\[-2ex]\n" inner + "\\rule{\\textwidth}{0.4pt}"
    | _ -> inner // TODO: add rest of commands, error out here when no commands are matches

(* convert list of formatted texts to one string *)
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

(* 
    convert an entire line to latex code, using recorded list caps
    to add list start/end commmands if that's necessary
*)
let rec evalLines (lines: Line list) (caps: ModuleCap list) : string =
    match lines, caps with
    | FormattedTexts(line) :: ls', Start :: caps' ->
        "\\vspace{-\\baselineskip}\n"
        + "\\begin{itemize}[itemsep=0pt, topsep=0pt]\n"
        + (evalFormattedTexts line)
        + "\n"
        + (evalLines ls' caps')
    | FormattedTexts(line) :: ls', End :: caps' ->
        "\\end{itemize}\n"
        + (evalFormattedTexts line)
        + "\\\\ \n"
        + (evalLines ls' caps')
    | FormattedTexts(line) :: ls', Nothing(isItem) :: caps' ->
        (evalFormattedTexts line)
        + (if isItem then "\n" else "\\\\ \n")
        + (evalLines ls' caps')
    | [], [] -> ""
    | [], _ -> "\\end{itemize}\n"
    | _ -> ""

(* evaluate txt file and convert to latex, compile latex if possible *)
let eval (ast: Expression) (file_name: string) : unit =
    let list_caps = find_list_caps ast

    match ast with
    | Lines(lines) ->
        let res = doc_preamble + (evalLines lines list_caps) + doc_ending
        let tex_file_name = file_name.Replace(".txt", ".tex")
        System.IO.File.WriteAllText(tex_file_name, res)

        let test_package =
            executeShellCommand "pdflatex --version" |> Async.RunSynchronously

        if test_package.StandardOutput.StartsWith("pdfTeX") then
            let dir_path = Path.GetDirectoryName(file_name)

            let output_command =
                if dir_path = "" then
                    ""
                else
                    (sprintf "-output-directory %s" dir_path)

            let command = sprintf "pdflatex -halt-on-error %s %s" output_command tex_file_name
            let compile_res = executeShellCommand command |> Async.RunSynchronously

            if compile_res.StandardOutput.Contains("Fatal error occurred") then
                printfn "Uh oh, something went wrong with your resume compilation!"
                printfn "Run `pdflatex %s` in the shell to get more info on the issue." tex_file_name
                exit 1
            else
                printfn "CARRER resume succesfully compiled to %s" (file_name.Replace(".txt", ".pdf"))
        else
            printfn "CARRER resume succesfully compiled to %s" tex_file_name
            printfn "Download pdflatex (or use overleaf online) to compile %s" tex_file_name
