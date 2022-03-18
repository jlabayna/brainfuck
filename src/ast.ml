type expr =
  | Inc
  | Dec
  | Movr
  | Movl
  | Loop of expr list
  | Write
  | Read

type prog = expr list
