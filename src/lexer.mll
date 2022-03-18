{
open Parser
}

let comment = [^ '+' '-' '<' '>' '[' ']' '.' ',']+

rule read =
  parse
  | comment { read lexbuf}
  | "+" { PLUS }
  | "-" { MINUS }
  | ">" { RANGLE }
  | "<" { LANGLE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "." { PERIOD }
  | "," { COMMA }
  | eof { EOF }
