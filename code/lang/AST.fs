module AST

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
