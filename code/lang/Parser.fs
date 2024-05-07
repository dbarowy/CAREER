module Parser

open Combinator

(* flag to print debug info *)
let DEBUG = false

(* Top level type, holds all lines in resume *)
type Expression = Lines of Line list

(* Represents one line in input/output resume *)
and Line = FormattedTexts of FormattedText list

(* Either a modifier or just raw text *)
and FormattedText =
    | Modifier of Modifier
    | String of string

(* some modifying function applied to formatted texts *)
and Modifier = string * (FormattedText list)

let expr, exprImpl = recparser ()
let formattedText, formattedTextImpl = recparser ()

(* special characters that need to be prefaced by / in latex *)
let backslashReservedCharacters = [ '#'; '$'; '%'; '&'; '~'; '_'; '^'; '{'; '}' ]
(* special characters reserved for CAREER usage *)
let careerReservedCharacters = [ '*'; '\n'; '['; ']' ]
(* special characters that need to be surrounded by $ in latex *)
let dolarReservedCharacters = [ '>'; '<'; '\\' ]


let backslashReservedChar =
    ((psat (fun (c) -> List.contains c backslashReservedCharacters))
     |>> (fun (c) -> (sprintf "\\%c" c)))

let dolarReservedChar =
    ((psat (fun (c) -> List.contains c dolarReservedCharacters))
     |>> (fun (c) -> if c = '\\' then "$\\backslash$" else (sprintf "$%c$" c)))

let preservedchar = (backslashReservedChar <|> dolarReservedChar) |>> String

(* any non-special character *)
let goodChar =
    (psat (fun (c) ->
        not (List.contains c careerReservedCharacters)
        && not (List.contains c backslashReservedCharacters)
        && not (List.contains c dolarReservedCharacters)))
    <!> "goodChar"

(* string composed of non-special characters *)
let pgoodstr = pmany1 goodChar |>> stringify |>> String <!> "pgoodstr"

(* combine list of Strings into one string *)
let rec formattedTextsToString fs =
    match fs with
    | String(s) :: fs' -> s + (formattedTextsToString fs')
    | Modifier(_) :: _ ->
        printf "Uh oh, an error occurred"
        exit 1
    | [] -> ""

(* tex formatted string *)
let formattedString =
    (pmany1 (pgoodstr <|> preservedchar) |>> formattedTextsToString |>> String)

(* ITEM modifier function (can only be applied at the start of a line) *)
let itemModifierFunction =
    pright (pchar '*') (pstr "ITEM") <!> "item fodifier function"

let subsectionTitleModifierFunction =
    pright (pchar '*') (pstr "SUBSECTION_TITLE")
    <!> "subsection title modifier function"

(* general accepted modifier function (can be applied anywhere) *)
let modifierFunction =
    pright
        (pchar '*')
        (pstr "HEADER"
         <|> pstr "SUBHEADER"
         <|> pstr "SECTION"
         <|> pstr "BOLD"
         <|> pstr "UNDERLINE"
         <|> pstr "LINK"
         <!> "modifierFunction")

(* formatted text limited by "" *)
let limitedFormattedText =
    pbetween (pchar '[') (pmany1 formattedText) (pchar ']') <!> "limitedText"

(* full line modified by ITEM function  *)
let itemModifier =
    pseq (pleft itemModifierFunction pws0) (limitedFormattedText <|> (pmany1 formattedText)) Modifier
    <!> "itemModifier"

let oneToNRepeats p n =
    (pbind (pmany1 p) (fun c -> if (c.Length <= n) then presult c else pzero))
    <!> "1-3 repeats"

let subsectionTitleContent =
    oneToNRepeats (pleft (pbetween (pchar '[') formattedString (pchar ']')) (pmany0 (pchar ' '))) 3
    <|> (formattedString |>> (fun (s) -> [ s ]))

let subsectionTitleModifier =
    pseq (pleft subsectionTitleModifierFunction pws0) subsectionTitleContent Modifier

(* general text sequence formatted by some modifier *)
let modifier =
    pseq (pleft modifierFunction pws0) (limitedFormattedText <|> (pmany1 formattedText)) Modifier
    <!> "modifier"

(* either a modified text sequence or a tex formatted string *)
formattedTextImpl := modifier <|> formattedString <!> "formattedText"

(* one line from/to input/output resume *)
let line =
    (itemModifier |>> (fun (f) -> FormattedTexts([ f ])))
    <|> (subsectionTitleModifier |>> (fun (f) -> FormattedTexts([ f ])))
    <|> ((pmany1 formattedText) |>> FormattedTexts)
    <!> "line"

(* list of all lines in input/output resume *)
exprImpl := pmany1 (pleft line (pmany1 (pchar '\n'))) |>> Lines <!> "expr"

let grammar = pleft expr peof <!> "grammar"

(* parser to convert from .txt to AST *)
let parse (input: string) : Expression option =
    let formattedInput = input + "\n"

    let i =
        if DEBUG then
            debug formattedInput
        else
            prepare formattedInput

    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(pos, rule) ->
        printfn "Invalid expression"
        let msg = sprintf "Cannot parse input at position %d in rule '%s'" pos rule

        let diag = diagnosticMessage 20 pos input msg
        printfn "%s" diag
        None
