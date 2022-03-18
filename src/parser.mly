%{
  open Ast
%}

(*

 Cmd  Effect                                 Equivalent in C
 ---  ------                                 ---------------
 +    Increases element under pointer        array[p]++;
 -    Decrases element under pointer         array[p]--;
 >    Increases pointer                      p++;
 <    Decreases pointer                      p--;
 [    Starts loop, counter under pointer     while(array[p]) {
 ]    Indicates end of loop                  }
 .    Outputs ASCII code under pointer       putchar(array[p]);
 ,    Reads char and stores ASCII under ptr  array[p]=getchar();

*)

%token PLUS
%token MINUS
%token RANGLE
%token LANGLE
%token RBRACKET
%token LBRACKET
%token PERIOD
%token COMMA
%token EOF

%start <Ast.prog> prog

%%

prog:
  | elist = list(expr); EOF { elist }

expr:
  | PLUS { Inc }
  | MINUS { Dec }
  | RANGLE { Movr }
  | LANGLE { Movl }
  | LBRACKET; elist = list(expr); RBRACKET { Loop (elist) }
  | PERIOD { Write }
  | COMMA { Read }
