
open Ast
open Format

(* Excepção levantada para assinalar um erro durante a interpretação *)
exception VarUndef of string
exception Tipagem of string * string

type value =
  | Vint of int
  | Vbool of bool
  | Vstring of string

let tabelaTipos = Hashtbl.create 100
let tabelaTiposFunc = Hashtbl.create 100

(* Obter o valor da inteiros -> usável para for é crescente ou decrescente e para space em arrays *)
let rec valor numb =
	match numb with
	| Const c -> begin
      match c with
      | Cint i -> i  
      | _ -> assert false
  	end
    | Binop (o, e1, e2)-> begin
	      match o with
	      | Sum -> (valor e1) + (valor e2)
	      | Sub -> (valor e1) - (valor e2)
	      | Mul -> (valor e1) * (valor e2)
	      | Div -> (valor e1) / (valor e2)
	      | _ -> assert false
    end
    | Var id -> 0
   	| _ -> assert false

(* Devolve um tipo associado a uma expressão *)
let rec expr = function
  	| Const c -> begin
		match c with
		| Cint i -> "int"   
		| Cbool b -> "bool"
		| Cfloat f -> "float"
		| Cstring s -> "string"
  	end
  	| Var v -> 
		begin
			(* Verificar se 'v' é uma variável ou uma função *) 
			try Hashtbl.find tabelaTipos v 
			with Not_found -> begin 
								try Hashtbl.find tabelaTiposFunc v 
								with Not_found -> raise (VarUndef v) 
							  end
							  
		end
  	| Binop (o, e1, e2)-> 
		begin
			let v1 = expr e1 in
			let v2 = expr e2 in
			match o with
			| Sum | Sub | Mul | Div -> 
				begin
					(* Avaliar tipo de 'e1' e 'e2' e verificar se são ambos int *)  
			  		if (compare v1 "int" <> 0) then raise (Tipagem ("int",v1))
				  	else if compare v1 v2 <> 0 then raise (Tipagem (v1,v2)); 
				  	"int"
				end
			| And | Or ->
				begin
					(* Avaliar tipo de 'e1' e 'e2' e verificar se são ambos bool *) 
				  	if (compare v1 "bool" <> 0) then raise (Tipagem ("bool",v1))
				  	else if compare v1 v2 <> 0 then raise (Tipagem (v1,v2)); 
				  	"bool"
				end
			| Less | Greater | LessEq | GreatEq | Equals | Diff ->
				begin
					(* Avaliar tipo de 'e1' e 'e2' e verificar se têm o mesmo tipo *) 
			  		if compare v1 v2 <> 0 then raise (Tipagem (v1,v2)); 
			  		"bool"
				end
			| FSum | FSub | FMul | FDiv -> 
				begin
					(* Avaliar tipo de 'e1' e 'e2' e verificar se são ambos float *) 
				  	if (compare v1 "float" <> 0) then raise (Tipagem ("float",v1))
				  	else if compare v1 v2 <> 0 then raise (Tipagem (v1,v2)); 
				  	"float"
				end
		end
   	| Not e ->
    	begin
    		(* Avaliar tipo de 'e' e verificar se é bool' *) 
	   	   	let tExp = expr e in
	   	   	if compare "bool" tExp <> 0 then raise (Tipagem ("bool",tExp));
	   	   	"bool"
    	end
   	| LetIn(id,e1,e2) -> 
   		(* Avaliar tipo de 'e1' e atribuir a 'id' *)
   		let v = expr e1 in
    	Hashtbl.add tabelaTipos id v;
    	(* Avaliar tipo de 'e2', sabendo o tipo de 'id' e verificar se este é mantido *)
   		let v = expr e2 in
   		v;
   	| Fun(id,e) ->
   		Hashtbl.add tabelaTiposFunc id "int";
   		(* Avaliar tipo de 'e1' e atribuir a 'id' *)
   		let v = expr e in
   		Hashtbl.replace tabelaTiposFunc id v;
   		"fun";
   	| App(id,e) -> begin
   		(* Verifica se 'id' de fun existe e se a sua expressão não tem erros *)
   		let v = expr (Var id) in
   		if compare "fun" v <> 0 then raise (Tipagem ("fun",v));
   		let v = expr e in
   		if compare "int" v <> 0 then raise (Tipagem ("int",v));
    	"int";
    end
    | ArrayI c -> begin
    	(* Verifica se 'c' é int *)
   		let v = expr c in
   		if compare "int" v <> 0 then raise (Tipagem ("int",v));
    	"array";
   	end  
   	| ArrayIGet(id,e)  -> begin
   		try 
	      	let tId = Hashtbl.find tabelaTipos id in
	      	if compare "array" tId <> 0 then raise (Tipagem ("array",tId));
	      	let tExp = expr e in
	      	if compare "int" tExp <> 0 then raise (Tipagem ("int",tExp));
	      	"int";
	    with Not_found -> raise (VarUndef id);
   	end  	
   	| InInt -> "int"
   	| InFloat -> "float"
    | InString -> "string"
   	

and stmt = function
  	| Let (id,e) ->
  		(* Associar 'id' ao tipo de 'e' *) 
	    let v = expr e in
	    Hashtbl.add tabelaTipos id v
  	| Set (id,e) -> 
  		begin
		    try
		    	(* Verificar, se existir, se 'id' e 'e' têm o mesmo tipo *)
		      	let tId = Hashtbl.find tabelaTipos id in 
		      	let tExp = expr e in
		      	if compare tId tExp <> 0 then raise (Tipagem (tId,tExp));
		    with Not_found -> raise (VarUndef id);
  		end
  	| ArrayISet (id,e1,e2) -> 
  	begin
    	try 
      		let tId = Hashtbl.find tabelaTipos id in 
      		if compare "array" tId <> 0 then raise (Tipagem ("array",tId));
      		let tExp1 = expr e1 in
      		let tExp2 = expr e2 in
      		if compare tExp1 "int" <> 0 then raise (Tipagem ("int",tExp1));
      		if compare tExp1 tExp2 <> 0 then raise (Tipagem (tExp1, tExp2));     		
    	with Not_found -> raise (VarUndef id);
  	end
  	| Unop (o,id) -> 
		begin
		   	try
		   		(* Verificar, se existir, se 'id' tem tipo int *) 
		      	let tId = Hashtbl.find tabelaTipos id in 
		      	if compare "int" tId <> 0 then raise (Tipagem ("int",tId));
	    	with Not_found -> raise (VarUndef id);
    	end
	| OpAct (o,id,e) -> 
		begin
		   	try
		   		(* Verificar, se existir, se 'id' tem tipo int e 'e' o mesmo tipo de 'id' *) 
		      	let tId = Hashtbl.find tabelaTipos id in
		      	let tExp = expr e in 
		      	if compare "int" tId <> 0 then raise (Tipagem ("int",tId)); 
		      	if compare tId tExp <> 0 then raise (Tipagem (tId,tExp));
	    	with Not_found -> raise (VarUndef id);
    	end
    | IfThen (e,s1) -> 
		begin
			(* Verificar se 'e' é do tipo bool *)
		   	let tExp = expr e in
		   	if compare "bool" tExp <> 0 then raise (Tipagem ("bool",tExp));
    	end
	| IfThenElse (e,s1,s2) -> 
		begin
			(* Verificar se 'e' é do tipo bool *)
		   	let tExp = expr e in
		   	if compare "bool" tExp <> 0 then raise (Tipagem ("bool",tExp));
    	end
    | While (e,s) -> 
		begin
			(* Verificar se 'e' é do tipo bool *)
		   	let tExp = expr e in
		   	if compare "bool" tExp <> 0 then raise (Tipagem ("bool",tExp));
    	end
  	| For  (Set (id,e),e1,e2,sFor) -> 
  		begin
		    try
		    (* Verificar, se existir, se 'id' tem o mesmo tipo de 'e' e se 'e1', 'e2' e 'e' têm o mesmo tipo *)
	      	let tId = Hashtbl.find tabelaTipos id in 
	      	let tExp = expr e in
	      	if compare tId "int" <> 0 then raise (Tipagem ("int",tId));
	      	if compare tId tExp <> 0 then raise (Tipagem (tId,tExp));
	      	let tExp1 = expr e1 in
	      	let tExp2 = expr e2 in
	      	if not (compare tExp1 tExp2 == 0 && compare tExp1 tExp == 0) then raise (Tipagem (tExp,tExp1));
		    with Not_found -> raise (VarUndef id);
  		end
  	| OutInt e ->
    	begin
    		(* Associar o tipo de 'e' a OutInt e verificar se 'e' tem tipo int *) 
			let v = expr e in
			(* Verificar se valor de 'e' tem o tipo inteiro *)
			if compare "int" v <> 0 then raise (Tipagem ("int",v));
    	end

  	| OutFloat e ->
  		begin
  			(* Associar o tipo de 'e' a OutFloat e verificar se 'e' tem tipo float *) 
		    let v = expr e in
		    (* Verificar se valor de 'e' tem o tipo inteiro *)
		    if compare "float" v <> 0 then raise (Tipagem ("float",v));
		end
  	| OutString e ->
  		begin
  			(* Associar o tipo de 'e' a OutString e verificar se 'e' tem tipo string *) 
		    let v = expr e in
		    (* Verificar se valor de 'e' tem o tipo inteiro *)
		    if compare "string" v <> 0 then raise (Tipagem ("string",v));
		end
   | _ -> raise (Tipagem ("não entrar aqui","entrei aqui!"));

