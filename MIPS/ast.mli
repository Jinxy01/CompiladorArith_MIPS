
(* sintaxe abstracta para a linguagem MIPS *)

(* type ident = string *)
type args = string list
type program = stmt list

and stmt = 
	| Let of string * expr
	| Set of string * expr
	| Unop of opu * string 
	| OpAct of opact * string * expr
	| OutInt of expr
	| OutFloat of expr
	| OutString of expr
	| ArrayISet of string * expr *  expr
	| IfThen of expr * stmt
	| IfThenElse of expr * stmt * stmt
	| ConjStmt of stmt * stmt
	| While of expr * stmt
	| For of stmt * expr * expr * stmt

and expr =
	| Const of cst
	| Var of string
	| Binop of op * expr * expr
	| LetIn of string * expr * expr
	| Fun of string * expr
	| App of string * expr
	| Not of expr
	| ArrayI of expr
	| ArrayIGet of string * expr
	| InInt
	| InFloat 
	| InString

and cst =
	| Cint of int
	| Cbool of bool
	| Cfloat of float
	| Cstring of string

and op = 
 	| Sum | Sub | Mul | Div 
 	| FSum | FSub | FMul | FDiv
 	| And | Or | Less | Greater | LessEq | GreatEq | Equals | Diff

and opu = 
	| PPlus | MMinus

and opact = 
 	| PlusEq | MinusEq | MulEq | DivEq	