module Parser

open Combinator

let DEBUG = false

type Expression = Lines of Line list

and Line = FormattedTexts of FormattedText list

and FormattedText =
    | Modifier of Modifier
    | String of string

and Modifier = string * (FormattedText list)

let expr, exprImpl = recparser ()
let formattedText, formattedTextImpl = recparser ()

let ppunc =
    pchar '.'
    <|> pchar '?'
    <|> pchar '!'
    <|> pchar ','
    <|> pchar '-'
    <|> pchar '—'
    <|> pchar ':'
    <|> pchar ';'

let goodChar = pletter <|> pdigit <|> ppunc <|> (pchar ' ') <!> "goodChar"
let pgoodstr = pmany1 goodChar |>> stringify |>> String <!> "pgoodstr"

let modifierFunction =
    pright
        (pchar '*')
        (pstr ("HEADER")
         <|> pstr ("HEADER_LEFT")
         <|> pstr ("HEADER_RIGHT")
         <|> pstr ("GENERIC_SECTION")
         <|> pstr ("TITLE_CENTER")
         <|> pstr ("TITLE_LEFT")
         <|> pstr ("TITLE_RIGHT")
         <|> pstr ("ITEM")
         <|> pstr ("BOLD")
         <|> pstr ("UNDERLINE")
         <!> "modifierFunction")

let limitedFormattedText = pbetween (pchar '"') (pmany1 formattedText) (pchar '"')

let modifier =
    pseq (pleft modifierFunction pws1) (limitedFormattedText <|> (pmany1 formattedText)) Modifier
    <!> "modifier"

formattedTextImpl := modifier <|> pgoodstr <!> "formattedText"

let line = pmany1 formattedText |>> FormattedTexts

exprImpl := pmany1 (pleft line (pmany1 (pchar '\n'))) |>> Lines <!> "expr"

let grammar = pleft expr peof <!> "grammar"

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

        // TODO: fix diagnostic message to print lines correctly
        let diag = diagnosticMessage 20 pos input msg
        printfn "%s" diag
        None
