module Parser

open Combinator

// TODO: add link modifier
// require quotes for link, only allow string (not modifier) within link, and then link can take any character

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

// TODO: try out just accepting all non-special characters using psat
(* non-alphabetic/numerical accepted characters *)
let ppunc =
    pchar '.'
    <|> pchar '?'
    <|> pchar '!'
    <|> pchar ','
    <|> pchar '-'
    <|> pchar '—'
    <|> pchar ':'
    <|> pchar ';'
    <|> pchar '('
    <|> pchar ')'
    <|> pchar '+'
    <|> pchar '/'
    <|> pchar '"'
    <|> pchar '”'
    <!> "ppunc"

(* special characters that need to be prefaced by / in latex *)
let pspecialchar =
    (pchar '#' <|> pchar '&') |>> (fun (c) -> (sprintf "\\%c" c)) |>> String

(* any non-special character *)
let goodChar = pletter <|> pdigit <|> ppunc <|> (pchar ' ') <!> "goodChar"

(* string composed of non-special characters *)
let pgoodstr = pmany1 goodChar |>> stringify |>> String <!> "pgoodstr"

(* combine list of Strings into one string *)
let rec formattedTextsToString fs =
    match fs with
    | String(s) :: fs' -> s + (formattedTextsToString fs')
    | Modifier(_) :: fs' ->
        printf "Uh oh, an error occurred"
        exit 1
    | [] -> ""

(* tex formatted string *)
let formattedString =
    (pmany1 (pgoodstr <|> pspecialchar) |>> formattedTextsToString |>> String)

(* ITEM modifier function (can only be applied at the start of a line) *)
let itemModifierFunction =
    pright (pchar '*') (pstr "ITEM") <!> "itemModifierFunction"

(* general accepted modifier function (can be applied anywhere) *)
let modifierFunction =
    pright
        (pchar '*')
        (pstr "HEADER"
         <|> pstr "HEADER_LEFT"
         <|> pstr "HEADER_RIGHT"
         <|> pstr "SECTION"
         <|> pstr "TITLE_CENTER"
         <|> pstr "TITLE_LEFT"
         <|> pstr "TITLE_RIGHT"
         <|> pstr "BOLD"
         <|> pstr "UNDERLINE"
         <!> "modifierFunction")

(* formatted text limited by "" *)
let limitedFormattedText =
    pbetween (pchar '"') (pmany1 formattedText) (pchar '"') <!> "limitedText"

(* full line modified by ITEM function  *)
let itemModifier =
    pseq (pleft itemModifierFunction pws1) (limitedFormattedText <|> (pmany1 formattedText)) Modifier
    <!> "itemModifier"

(* general text sequence formatted by some modifier *)
let modifier =
    pseq (pleft modifierFunction pws1) (limitedFormattedText <|> (pmany1 formattedText)) Modifier
    <!> "modifier"

(* either a modified text sequence or a tex formatted string *)
formattedTextImpl := modifier <|> formattedString <!> "formattedText"

(* one line from/to input/output resume *)
let line =
    (itemModifier |>> (fun (f) -> FormattedTexts([ f ])))
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
