open Ast

(* Declare brainfuck array/tape and pointer*)
let size = 30000
let tape = Array.make size 0
let ptr = ref 0

(** Take a list of expressions an evaluate them *)
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

(** Parse a string s into an ast as defined in ast.ml *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** Interpret an input brainfuck string *)
let interp (e:string) : unit =
  e |> parse |> eval_prog;
  for i = 0 to (size - 1) do
    tape.(i) <- 0
  done
