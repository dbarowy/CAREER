module Evaluator

open AST
open Shell
open System.IO
open System.Text.RegularExpressions

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
    [ "\\documentclass[font_size]{extarticle}" // will replace later with true font size
      "\\usepackage{enumitem}"
      "\\usepackage{geometry}"
      "\\usepackage{hyperref}"
      "\\usepackage{xcolor}"
      "\\hypersetup{colorlinks,urlcolor={blue!80!black}}" // remove ugly neon box around links, make them blue instead
      "\\geometry{top=.75in, bottom=.75in, left=.5in, right=.5in}" // reduce margins
      "\\geometry{paperwidth=8.5in, paperheight=11in}"
      "\\setlength{\\parindent}{0pt}"
      "\\begin{document}"
      "\\thispagestyle{empty}" ]
    |> List.fold (fun acc s -> acc + s + "\n") ""

(* Ending to output latex file *)
let doc_ending = "\\end{document}\n"

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

(* find the start and end of all lists, and record them for later use *)
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
        + sprintf "\\Large \\textbf{%s}\n" inner
        + "\\end{center}\n"
        + "\\vspace{-6.5ex}~"
    | "SUBHEADER" ->
        "\\vspace{-\\baselineskip}\n"
        + "\\begin{center}\n"
        + sprintf "\\normalsize %s\n" inner
        + "\\end{center}\n"
        + "\\vspace{-6ex}~"
    | "SECTION" -> sprintf "\\textbf{%s}\\\\[-2ex]\n" inner + "\\rule{\\textwidth}{0.4pt}"
    | "ITEM" -> sprintf "\\item %s" inner
    | "BOLD" -> sprintf "\\textbf{%s}" inner
    | "UNDERLINE" -> sprintf "\\underline{%s}" inner
    | "LINK" -> sprintf "\\url{%s}" inner
    | _ -> inner

(* 
    evaluate a subsection title.
    1 argument  -> bol + underlined center alligned text
    2 arguments -> bold text alligned left, bold text alligned right
    3 arguments -> bold text alligned left, bold + underligned text center alligned
                   bold text alligned right
*)
let rec evalSubsectionTitle (formattedTexts: FormattedText list) =
    let rec textsToString texts =
        match texts with
        | String(s) :: texts' -> s :: textsToString texts'
        | _ :: texts' ->
            printfn "Subsection Titles should only contain strings (no modifiers)"
            exit 1
        | [] -> []

    let stringTexts = textsToString formattedTexts

    let res =
        match stringTexts.Length with
        | 1 ->
            "\\begin{minipage}[t]{\\textwidth}\n"
            + "\\centering\n"
            + (sprintf "\\textbf{\\underline{%s}}\n" stringTexts[0])
            + "\end{minipage}%\n"
        | 2 ->
            "\\begin{minipage}[t]{0.5\\textwidth}\n"
            + "\\raggedright\n"
            + (sprintf "\\textbf{%s}\n" stringTexts[0])
            + "\end{minipage}%\n"
            + "\\begin{minipage}[t]{0.5\\textwidth}\n"
            + "\\raggedleft\n"
            + (sprintf "\\textbf{%s}\n" stringTexts[1])
            + "\end{minipage}%\n"
        | 3 ->
            "\\begin{minipage}[t]{0.333\\textwidth}\n"
            + "\\raggedright\n"
            + (sprintf "\\textbf{%s}\n" stringTexts[0])
            + "\end{minipage}%\n"
            + "\\begin{minipage}[t]{0.333\\textwidth}\n"
            + "\\centering\n"
            + (sprintf "\\textbf{\\underline{%s}}\n" stringTexts[1])
            + "\end{minipage}%\n"
            + "\\begin{minipage}[t]{0.333\\textwidth}\n"
            + "\\raggedleft\n"
            + (sprintf "\\textbf{%s}\n" stringTexts[2])
            + "\end{minipage}%\n"
        | _ ->
            printfn "Subsection Titles should have between 1 and 3 items"
            exit 1

    "\\begin{tabular}{@{}p{\\textwidth}}"
    + res
    + "\\end{tabular}\\\\[0.5ex]\n"
    + "%"

(* convert list of formatted texts to one string *)
let rec evalFormattedTexts (formattedTexts: FormattedText list) =
    let rec evalFormattedText (f: FormattedText) =
        match f with
        | Modifier(s, f) when s = "SUBSECTION_TITLE" -> evalSubsectionTitle f
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
        "\\end{itemize}~\\\\[-1ex]\n"
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

(* scan pdflatex output to get the pdf page length *)
let get_output_page_length (output: string) =
    let pattern = @"(\d+) pages,"
    let res = Regex.Match(output, pattern)

    if res.Success then
        Some(int (res.Groups[1].Value))
    else
        None

(* 
    Compile ast to .tex file, and compile .tex file using pdflated
    if the user has pdflatex installed.
    Automatically re-compile with a smaller font (down to size 8 font)
    if the resume pdf is over 1 page long.
*)
let rec compile_output (lines: Line list) (list_caps: ModuleCap list) (file_name: string) (font_size: int) =
    // latex preamble with correct font size
    let formatted_preamble =
        doc_preamble.Replace("documentclass[font_size]", (sprintf "documentclass[%dpt]" font_size))

    // create formatted latex file, output to filename.tex
    let res = formatted_preamble + (evalLines lines list_caps) + doc_ending
    let tex_file_name = file_name.Replace(".txt", ".tex")
    System.IO.File.WriteAllText(tex_file_name, res)

    // test if user has pdflatex installed
    let test_for_pdflatex =
        executeShellCommand "pdflatex --version" |> Async.RunSynchronously

    // user hsa pdflatex installed
    if test_for_pdflatex.StandardOutput.StartsWith("pdfTeX") then
        let dir_path = Path.GetDirectoryName(file_name)

        let output_command =
            if dir_path = "" then
                ""
            else
                // pdflatex requires a special flag to output in the same directory as the input
                (sprintf "-output-directory %s" dir_path)

        let command = sprintf "pdflatex -halt-on-error %s %s" output_command tex_file_name
        let compile_res = executeShellCommand command |> Async.RunSynchronously

        if compile_res.StandardOutput.Contains("Fatal error occurred") then
            printfn "Uh oh, something went wrong with your resume compilation!"
            printfn "Run `pdflatex %s` in the shell to get more info on the issue." tex_file_name
            exit 1
        else
            let res = get_output_page_length compile_res.StandardOutput

            let sys_message =
                sprintf "CARRER resume succesfully compiled to %s" (file_name.Replace(".txt", ".pdf"))

            match res with
            | Some(num) ->
                // any font size less than 8 isn't supported by latex (and would probably look bad anyways)
                if num > 1 && font_size > 8 then
                    compile_output lines list_caps file_name (font_size - 1)
                else
                    printfn "%s" sys_message
            | None -> printfn "%s" sys_message
    else
        printfn "CARRER resume succesfully compiled to %s" tex_file_name
        printfn "Download pdflatex (or use overleaf) to compile %s" tex_file_name

(* evaluate txt file and convert to latex, compile latex if possible *)
let eval (ast: Expression) (file_name: string) : unit =
    let list_caps = find_list_caps ast

    match ast with
    | Lines(lines) -> compile_output lines list_caps file_name 10
