
(* Lexer para Arith *)

{
  open Lexing
  open Parser

  exception Lexing_error of char

  let kwd_tbl = ["outputInt",OUTPUT_INT;
             	"outputFloat",OUTPUT_FLOAT;
             	"outputString",OUTPUT_STRING;
             	"inputInt",INPUT_INT;
             	"inputFloat",INPUT_FLOAT;
             	"inputString",INPUT_STRING;
    			"let", LET;
    			"in", IN;
             	"if", IF;
             	"then", THEN;
             	"else", ELSE;
             	"while", WHILE;
             	"for", FOR;
             	"fun", FUN;
             	"not", NOT;
             	"arrayInt", ARRAYINT;
             	]

  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let bool = "true" | "false"
let int = ['0'-'9']+
let float = digit digit* ['.'] digit* | digit digit* ['.'] digit*
let char = letter
let space = [' ' '\t']
let string = '"'(((letter|digit|space) (letter|digit|space)+) | digit)'"'


rule token = parse
  | '\n'    				{ newline lexbuf; token lexbuf }
  | "#" [^'\n']* '\n' 		{ newline lexbuf; token lexbuf }
  | space+  				{ token lexbuf }
  | '='						{ EQ }
  | '+' 					{ SUM }
  | '-' 					{ SUB }
  | '*' 					{ MUL }
  | '/' 					{ DIV }
  | "+."          			{ FSUM }
  | "-."          			{ FSUB }
  | "*."          			{ FMUL }
  | "/."          			{ FDIV }
  | '(' 					{ LP }
  | ')' 					{ RP }
  | "&&"					{ AND }
  | "||" 					{ OR }
  | '<'           			{ LESS }
  | '>'           			{ GREATER }
  | "<="          			{ LESSEQ }
  | ">="          			{ GREATEQ }
  | "=="          			{ EQUALS }
  | "<>"          			{ DIFF }
  | ';'           			{ SEMICOL }
  | '{'           			{ DO }
  | '}'           			{ DONE }
  | "<-"          			{ ASSIGN }
  | ':'           			{ DOUBLEDOT }
  | "->"          			{ RARROW }
  | "+="          			{ PLUSEQ }
  | "-="          			{ MINUSEQ }
  | "*="          			{ MULEQ }
  | "/="          			{ DIVEQ }
  | "++"          			{ PPLUS }
  | "--"          			{ MMINUS }
  | '['           			{ LB }
  | ']'           			{ RB }
  | "(*"              		{ comment lexbuf }	
  | int as s 	  			{ CST (Cint (int_of_string s)) }
  | bool as s 				{ CST (Cbool (bool_of_string s)) }
  | float as s    			{ CST (Cfloat (float_of_string s)) }
  | string as s    			{ CST (Cstring (s)) }
  | ident as id 			{ id_or_kwd id }
  | eof     		  		{ EOF }
  | _ as c  				{ raise (Lexing_error c) }

and comment = parse
  | "*)"  { token lexbuf }
  | eof   { raise(Lexing_error 'E') }
  | _     { comment lexbuf }
