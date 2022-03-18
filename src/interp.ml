(*type env = int array
type reader = (env, int) -> ()*)
open Ast

let tape = Array.make 30000 0
let ptr = ref 0

let rec eval_prog : expr list -> unit = fun elist ->
  match elist with
  | Inc::tl -> tape.(!ptr) <- tape.(!ptr) + 1; eval_prog tl
  | Dec::tl -> tape.(!ptr) <- tape.(!ptr) - 1; eval_prog tl
  | Movr::tl -> ptr := !ptr + 1; eval_prog tl
  | Movl::tl -> ptr := !ptr - 1; eval_prog tl
  | (Loop el)::tl ->
      let cond = !ptr in
      while tape.(cond) != 0 do
        eval_prog el
      done; eval_prog tl
  | Write::tl -> print_char (char_of_int tape.(!ptr)); eval_prog tl
  | Read::tl ->
      tape.(!ptr) <-
        int_of_char (Scanf.bscanf Scanf.Scanning.stdin "%c" (fun x -> x));
        eval_prog tl
  | [] -> ()

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let interp (e:string) : unit =
  e |> parse |> eval_prog

