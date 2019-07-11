
/* parser para  Arith */

%{
  open Ast
%}


%token <Ast.cst> CST
%token <string> IDENT
%token EOF
%token LET EQ 
%token ASSIGN
%token IN LP RP
%token IF THEN ELSE
%token SEMICOL
%token WHILE DO DONE
%token FOR DOUBLEDOT
%token FUN RARROW
%token NOT
%token ARRAYINT
%token RB LB
%token OUTPUT_INT OUTPUT_FLOAT OUTPUT_STRING	
%token INPUT_INT INPUT_FLOAT INPUT_STRING

%token SUM SUB MUL DIV
%token FSUM FSUB FMUL FDIV
%token AND OR LESS GREATER LESSEQ GREATEQ EQUALS DIFF

%token PPLUS MMINUS
%token PLUSEQ MINUSEQ MULEQ DIVEQ
/* Definição das prioridades e associatividades dos tokens */

%nonassoc IDENT
%left AND OR LESS GREATER LESSEQ GREATEQ EQUALS DIFF RARROW ARRAYINT
%left SUM SUB FSUM FSUB 
%left MUL DIV FMUL FDIV
%left THEN
%left ELSE
%left SEMICOL NOT
%nonassoc uminus

/*Maior prioridade*/

/* Ponto de entrada da gramática */
%start prog

/* Tipo dos valores devolvidos pelo parser */
%type <Ast.program> prog

%%

prog:
| p = stmts EOF { List.rev p }
;

stmts:
	| i = stmt 		            { [i] }
	| l = stmts i = stmt 		{ i :: l }
;
	
stmt: 
	| LET id = IDENT EQ e = expr																		{ Let (id, e) }
	| id = IDENT ASSIGN e = expr																		{ Set (id, e) }
	| id = IDENT o = opu																				{ Unop (o, id) }
	| id = IDENT o = opact e = expr																		{ OpAct (o, id, e) }
	| IF e = expr THEN s = stmt 																		{ IfThen (e,s) }
	| IF e = expr THEN s1 = stmt ELSE s2 = stmt															{ IfThenElse (e,s1,s2) }
	| s1 = stmt SEMICOL s2 = stmt           															{ ConjStmt (s1,s2) }
	| LP s = stmt RP           																			{ s } 
	| WHILE e = expr DO s = stmt DONE          															{ While(e,s) } 
	| FOR LP id = IDENT ASSIGN e1 = expr DOUBLEDOT e2 = expr DOUBLEDOT e3 = expr RP DO s = stmt DONE   	{ For(Set(id,e1),e2,e3,s) }
	| id = IDENT LB e1 = expr RB ASSIGN e2 = expr														{ ArrayISet (id,e1,e2) } 
	| OUTPUT_INT e = expr 																				{ OutInt e }
	| OUTPUT_FLOAT e = expr 																			{ OutFloat e }
	| OUTPUT_STRING e = expr 																			{ OutString e }
;	

expr: c = CST 											{ Const c }
	| id = IDENT										{ Var id }
	| LP e = expr RP									{ e }
	| e1 = expr o = op e2 = expr						{ Binop (o, e1, e2) }
	| NOT e = expr										{ Not e }
	| FUN id = IDENT RARROW e = expr					{ Fun(id,e) }
	| LP id = IDENT e = expr RP							{ App(id,e) }
	| LET id = IDENT EQ e1 = expr IN LP e2 = expr RP	{ LetIn (id, e1, e2) }
	| ARRAYINT e = expr									{ ArrayI e }
	| id = IDENT LB e = expr RB 						{ ArrayIGet(id,e) }
	| INPUT_INT 										{ InInt }
	| INPUT_FLOAT 										{ InFloat }
	| INPUT_STRING 										{ InString }
	| SUB e = expr %prec uminus    						{ Binop (Sub, Const (Cint 0), e) }
	| FSUB e = expr %prec uminus    					{ Binop (FSub, Const (Cfloat 0.0), e) }
;

%inline op: SUM							{ Sum }
		  | SUB 						{ Sub }
		  | MUL							{ Mul }
		  | DIV							{ Div }
		  | FSUM						{ FSum }
		  | FSUB 						{ FSub }
		  | FMUL						{ FMul }
		  | FDIV						{ FDiv }
		  | AND							{ And }
		  | OR							{ Or }
		  | LESS						{ Less }
		  | GREATER 					{ Greater }
		  | LESSEQ						{ LessEq }
		  | GREATEQ						{ GreatEq }
		  | EQUALS						{ Equals }
		  | DIFF						{ Diff }
;
%inline opu: PPLUS						{ PPlus }
		  | MMINUS						{ MMinus }
;

%inline opact: PLUSEQ					{ PlusEq }
			| MINUSEQ					{ MinusEq }
			| MULEQ						{ MulEq }
			| DIVEQ						{ DivEq }