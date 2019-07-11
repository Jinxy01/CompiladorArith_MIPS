
(* Produção de código para a linguagem Arith *)

open Format
open MIPS
open Ast
open Tipagem

(* Exceção por lançar caso o utilizador aceda a uma posição incorreta de memória *)
exception OutOfBounds of string

(* Tamanho em byte da frame (cada variável local ocupa 8 bytes) *)
let frame_size = ref 0

(* As funções a colocar no final do programa *)
let (genfunc : (string, string) Hashtbl.t) = Hashtbl.create 100
(* As variáveis globais estão arquivadas numa HashTable *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 100
(* Os flutuantes a declarar no data *)
let (genf : (string, float) Hashtbl.t) = Hashtbl.create 100
(* As string a declarar no data *)
let (gens : (string, string) Hashtbl.t) = Hashtbl.create 100
(* Os espaços a reservar no data *)
let (genSpace : (string, int) Hashtbl.t) = Hashtbl.create 100
(* Associa id de array introduzido automaticamente ao id metido por user *)
let (genArrayAssoc : (string, string) Hashtbl.t) = Hashtbl.create 100

(* Contadores auxiliares para definição de labels *)
let arrayCounter = ref 0
let sInCounter = ref 0
let sCounter = ref 0 
let fCounter = ref 0 
let ifCounter = ref 0
let whileCounter = ref 0 
let forCounter = ref 0 
let funcCounter = ref 0 
let andCounter = ref 0
let orCounter = ref 0

module StrMap = Map.Make(String)

let compile_expr =
  let rec comprec env next = function
    | Const c -> begin
      match c with
      | Cint i ->
        li (reg t0) (imm i) ++              (* Colocar valor de i em t0 *)
        push t0                 			(* Colocar valor em t0 na pilha *)    
      | Cbool b ->
      	begin
	      	match b with
	      	| true -> li (reg t0) (imm 1) ++ push t0	(* Colocar valor 1 (true) em t0 e colocar t0 na pilha *)
	        | false -> li (reg t0) (imm 0) ++ push t0	(* Colocar valor 0 (false) em t0 e colocar t0 na pilha *)
    	end
      | Cfloat f ->
        fCounter := !fCounter + 1;									
      	Hashtbl.add genf ("vFloat"^(string_of_int !fCounter)) f;	(* Associar o nome da variável ao número *)
        la (reg t0) (lab ("vFloat"^(string_of_int !fCounter))) ++   (* Carregar o endereço da variável para t0 *)     
        flw (reg f4) (ind t0) ++									(* Fazer loadword, especifico de float, de t0*)    																	
        fpush f4													(* para f4 e colocar valor em f4 na pilha *)
      | Cstring s ->
      	sCounter := !sCounter + 1;										
      	let word = String.sub s 1 ((String.length s) - 2) in 			(* Obter string sem "" e associar o nome da *)
      	Hashtbl.add gens ("vString"^(string_of_int !sCounter)) word;	(* variável à string obtida *)
      	la (reg t0) (lab ("vString"^(string_of_int !sCounter))) ++ 		(* Carregar o endereço da variável para t0 *)
        push t0 														(* Colocar valor em t0 na pilha *)
        
    end
    | Var x -> begin
          try
            let ofs = - (StrMap.find x env) in  	(* Obter o offset associado à variável local x *)
            lw (reg t0) (ind ~ofs fp) ++			(* Fazer loadword do valor da variável para t0 *)
            push t0 								(* Colocar valor em t0 na pilha *)
            (* Caso a variável não seja local chegamos a Not_Found *)
          with Not_found ->
            lw (reg t0) (lab x) ++ 					(* Fazer loadword do valor da variável para t0 *)
            push t0 								(* Colocar valor em t0 na pilha *)
        end

    | Binop (o, e1, e2)-> 
    	begin
	        let op = match o with                   
	          | Sum -> add
	          | Sub -> sub
	          | Mul -> mul
	          | Div -> div
	          | FSum -> fadd
	          | FSub -> fsub
	          | FMul -> fmul
	          | FDiv -> fdiv
	          | And -> mand
	          | Or -> mor
	          | Less -> slt
	          | Greater -> sgt
	          | LessEq -> sle
	          | GreatEq -> sge
	          | Equals -> seq
	          | Diff -> sne
	        in
	    	begin match o with
	    	(* Realizar operações com flutuantes *)
	    	| FSum | FSub | FMul | FDiv -> 
		    	begin
			        comprec env next e1 ++                  
	        		comprec env next e2 ++
		    		fpop f6 ++
			        fpop f5 ++                  
			        op (reg f4) (reg f5) (reg f6) ++               
			        fpush f4
		    	end
	    	(* Avaliação preguiçosa *)
		   	| And ->
		   		begin
		   			andCounter := !andCounter + 1;
		   			let numAnd = !andCounter in
		   			comprec env next e1 ++
		   			pop t1 ++
		   			(* Se primeira expressão der false retornar esse valor (jump para andFalse) *)
		   			beq (reg t1) (reg zero) (lab ("andFalse"^(string_of_int numAnd))) ++
	        		comprec env next e2 ++
	        		pop t2 ++
	        		op (reg t0) (reg t1) (reg t2) ++               
			        push t0 ++
			        j (lab ("andFim"^(string_of_int numAnd)))++
			        label ("andFalse"^(string_of_int numAnd)) ++
			        push t1 ++
			        label ("andFim"^(string_of_int numAnd))
		   		end
		   	(* Avaliação preguiçosa *)
	   		| Or ->
		   		begin
		   			orCounter := !orCounter + 1;
		   			let numOr = !orCounter in
		   			comprec env next e1 ++
		   			pop t1 ++
		   			(* Se primeira expressão der true retornar esse valor (jump para orTrue) *)
		   			bne (reg t1) (reg zero) (lab ("orTrue"^(string_of_int numOr))) ++
	        		comprec env next e2 ++
	        		pop t2 ++
	        		op (reg t0) (reg t1) (reg t2) ++               
			        push t0 ++
			        j (lab ("orFim"^(string_of_int numOr)))++
			        label ("orTrue"^(string_of_int numOr)) ++
			        push t1 ++
			        label ("orFim"^(string_of_int numOr))
		   		end	   	
	    	| _ ->
		    	begin
		    		comprec env next e1 ++                  
	        		comprec env next e2 ++
		    		pop t2 ++
			        pop t1 ++                  
			        op (reg t0) (reg t1) (reg t2) ++               
			        push t0   
		    	end
			end 
		end
	| LetIn (x, e1, e2) -> begin
        if !frame_size = next then frame_size := 8 + !frame_size;
        comprec env next e1 ++                         
        pop t1 ++  
        sw (reg t1) (ind ~ofs:(-next) fp) ++                                      
        comprec (StrMap.add x next env) (next + 8) e2
    end
  	| InInt ->
		(* Preparar para ler inteiro *)
		li (reg v0) (imm 5) ++  
		syscall ++
		move (reg t0) (reg v0) ++
		push t0;  

    | InFloat ->
		(* Preparar para ler float *)
		li (reg v0) (imm 6) ++  
		syscall ++
		fmove (reg f4) (reg f0) ++
		fpush f4; 

    | InString ->
		sInCounter := !sInCounter + 1;
		let numCounter = !sInCounter in
		(* Preparar para ler string *)
		Hashtbl.add genSpace ("buffer"^(string_of_int numCounter)) 50;	(* Local para onde vai ser lida a string *)
		li (reg v0) (imm 8) ++  
		la (reg a0) (lab ("buffer"^(string_of_int numCounter))) ++
		li (reg a1) (imm 50) ++
		syscall ++
		move (reg t0) (reg a0) ++  
		push t0;

  	| Not e as instr ->
		(* Avaliar se 'e' é booleano; caso haja conflito, tipagem.ml levanta exceção *) 
		let _ = Tipagem.expr instr in
		comprec env next e ++
		pop t1 ++
		(* Verificar o valor associado a 'e': false -> 0, true -> 1 *)
		seq (reg t0) (reg t1) (reg zero) ++ 
		(* t0 terá o valor inverso de t1 *)  
		push t0

  	| ArrayI c as instr -> 
		arrayCounter := !arrayCounter + 1;
		let arrayCounter = !arrayCounter in
		(* Avaliar se 'c' é inteiro; caso haja conflito, tipagem.ml levanta exceção *)
		let _ = Tipagem.expr instr in
		comprec env next c ++
		pop t0 ++
		let vC = Tipagem.valor c in
    if vC <= 0 then raise (OutOfBounds "Valor inválido para declaração de array");
		Hashtbl.add genSpace ("array"^(string_of_int arrayCounter)) (vC*4); Nop  

  	| ArrayIGet(id, e) as instr ->
  	  	let _ = Tipagem.expr instr in
  	  	let vExp = Tipagem.valor e in
  	  	let arrayId = Hashtbl.find genArrayAssoc id in
		(* Obter o número de elementos do array *)
  	  	let sizeArray = Hashtbl.find genSpace arrayId in
		    let maxNumber = sizeArray/ 4 in 
		    if (vExp >= maxNumber || vExp < 0) then raise(OutOfBounds "Tentou aceder a uma posição de memória inválida");
  	  	comprec env next e ++
      	pop t1 ++

      	la (reg t3) (lab arrayId) ++
      	li (reg t0) (imm 4) ++
      	mul (reg t1) (reg t1) (reg t0) ++
      	add (reg t3) (reg t3) (reg t1) ++
  	  	lw (reg t0) (ind t3) ++
  	  	push t0

  	| Fun (id,e) as instr ->
		let _ = Tipagem.expr instr in
		funcCounter := !funcCounter + 1;
		let numFun = !funcCounter in
		Hashtbl.replace genv id ();
		j (lab ("funAntes"^(string_of_int numFun))) ++
		label ("Fun"^id) ++
		pop t0 ++
		sw (reg t0) (lab id) ++
		(* Avaliar expresão *)
		comprec env next e ++
		jr (reg ra) ++
		label ("funAntes"^(string_of_int numFun))

    | App(id,e) as instr -> 
		let _ = Tipagem.expr instr in
		let idFunc = Hashtbl.find genfunc id in
		comprec env next e ++ 
		jal (lab ("Fun"^idFunc));	  

   in comprec StrMap.empty 0

(* Compilação de uma instrução *)
let rec compile_instr = function
  	| Let (x, e) as instr ->
		let code =
		(* Avaliar expresão *)
		compile_expr e ++
		pop t0 ++
		sw (reg t0) (lab x) 
		in
		(* Guardar tipo de x em ambiente, em tipagem.ml *)
		let () = Tipagem.stmt instr in
		Hashtbl.replace genv x ();

		begin match e with
			(* Verificar se 'e' tem tipo 'fun' e associar id de fun a 'x' *)
		  	| Fun(id,_) -> Hashtbl.add genfunc x id
		  	(* Verificar se 'e' tem tipo 'var' e associar id de var a 'x' *)
		  	| Var id as exp ->  begin let v = Tipagem.expr exp in
	  			  				if (compare v "fun" == 0) then 
					  			let ident = Hashtbl.find genfunc id in
					  			Hashtbl.add genfunc x ident else 
					  			if compare v "array" == 0 then 
					  			let ident = Hashtbl.find genArrayAssoc id in
					  			Hashtbl.add genArrayAssoc x ident end

			(* Verificar se 'e' tem tipo 'array' e associar id de array a 'x' *)
		  	| ArrayI c -> Hashtbl.add genArrayAssoc x ("array"^(string_of_int !arrayCounter))
		  	| _ -> ()			
		end;
		code
  	| Set (x, e) as instr ->
     (* Avaliar se tipo de 'e' condiz com tipo de 'x', se este estiver definido; caso haja conflito, tipagem.ml levanta exceção *)
     let _ = Tipagem.stmt instr in
     (* Avaliar expresão *)
      let code =
        compile_expr e ++
        pop t0 ++
        sw (reg t0) (lab x) 
      in
      (* Substitui valor de x em ambiente *)
      Hashtbl.replace genv x ();
      (* Atualizar id associado de fun a 'x' *)
      begin match e with
	      | Fun(id,_) -> Hashtbl.replace genfunc x id
	      | _ -> ()
      end;
      code

  	| ArrayISet (id, e1, e2) as instr ->
  		(* Tipagem de array e obtencao de valor de v1 *)
      	let _ = Tipagem.stmt instr in
      	let vExp = Tipagem.valor e1 in
      	let arrayId = Hashtbl.find genArrayAssoc id in
		  (* Obter o número de elementos do array *)
      	let sizeArray = Hashtbl.find genSpace arrayId in
		    let maxNumber = sizeArray/ 4 in 
		    if (vExp >= maxNumber || vExp < 0) then raise(OutOfBounds "Tentou aceder a uma posição de memória inválida");
      	(* Avaliar expresão *)
      	compile_expr e1 ++
      	compile_expr e2 ++
      	pop t2 ++
      	pop t1 ++

  	  	let arrayId = Hashtbl.find genArrayAssoc id in
      	la (reg t3) (lab arrayId) ++
      	li (reg t0) (imm 4) ++
     	  mul (reg t1) (reg t1) (reg t0) ++
      	add (reg t3) (reg t3) (reg t1) ++
  	  	sw (reg t2) (ind t3);

  	| Unop (o, id) as instr-> 
    begin
    	(* Avaliar se 'id' é identificador; caso haja conflito, tipagem.ml levanta exceção *)
    	let _ = Tipagem.stmt instr in
    	lw (reg t0) (lab id) ++                  
        begin match o with                   
          | PPlus -> addiu (reg t0) (reg t0) (imm 1) ++ sw (reg t0) (lab id)
          | MMinus -> addiu (reg t0) (reg t0) (imm (-1)) ++ sw (reg t0) (lab id)
        end    
	end
  	| OpAct (o, id, e) as instr ->
  	begin
	  	(* Avaliar se 'id' é identificador e 'e' do mesmo tipo de 'id'; caso haja conflito, tipagem.ml levanta exceção *) 
	  	let _ = Tipagem.stmt instr in
	  	compile_expr e ++
	  	pop t1 ++
	  	lw (reg t0) (lab id) ++
	  	let op = 
	  		begin match o with                   
				| PlusEq -> add
				| MinusEq -> sub
				| MulEq -> mul
				| DivEq -> div
	    	end
    	in
	    op (reg t0) (reg t0) (reg t1) ++ 
	    sw (reg t0) (lab id)
	end
  	| ConjStmt (s1, s2) ->
      let codStmt1 = compile_instr s1 in
      let codStmt2 = compile_instr s2 in
      (* Executar uma instrução e depois outra *)
      codStmt1 ++
      codStmt2 

  	| IfThen (e,s) as instr ->
      ifCounter := !ifCounter + 1;
      let numCond = !ifCounter in
      (* Avaliar tipo de instrução *)
      let _ = Tipagem.stmt instr in
      (* Avaliar valor de expressão *)
      compile_expr e ++
      pop t0 ++
      (* Verificar se expressão é true ou false *)
      beq (reg t0) (reg zero) (lab ("condContinuar"^(string_of_int numCond))) ++
      (* Executar instrucao *)
      let codStmt = compile_instr s in
      codStmt ++
      (* Criar label para programa migrar para aqui, caso expressão dê falso *)
      label ("condContinuar"^(string_of_int numCond)) 

  	| IfThenElse (e,s1,s2) as instr ->
      ifCounter := !ifCounter + 1;
      let numCond = !ifCounter in
      (* Avaliar tipo de instrução *)
      let _ = Tipagem.stmt instr in
      (* Avaliar valor de expressão *)
      compile_expr e ++
      pop t0 ++
      (* Verificar se expressão é true ou false *)
      beq (reg t0) (reg zero) (lab ("Else"^(string_of_int numCond))) ++
      (* Executar instrucao de if e else *)
      let codStmtIf = compile_instr s1 in
      let codStmtElse = compile_instr s2 in
      (* Criar labels para if e else, colocando as instruções relativas a cada um (CodSmtIf e CodStmtElse) *)
      label ("If"^(string_of_int numCond)) ++
      codStmtIf ++
      j (lab ("condContinuar"^(string_of_int numCond))) ++
      label ("Else"^(string_of_int numCond)) ++
      codStmtElse ++
      j (lab ("condContinuar"^(string_of_int numCond))) ++
      (* Criar label para programa migrar para aqui, no final de if e else *)
      label ("condContinuar"^(string_of_int numCond)) 

  	| While (e,s) as instr ->
      whileCounter := !whileCounter + 1;
      let numLoop = !whileCounter in
      (* Avaliar tipo de instrução *)
      let _ = Tipagem.stmt instr in
      (* Avaliar valor de expressão *)
      compile_expr e ++
      pop t0 ++
      (* Verificar se expressão é true ou false *)
      beq (reg t0) (reg zero) (lab ("wContinuar"^(string_of_int numLoop))) ++
      (* Executar instrucao *)
      let codStmt = compile_instr s in
      (* Criar label para while *)
      label ("While"^(string_of_int numLoop)) ++
      codStmt ++
      (* Avaliar novamente valor de expressão *)
      compile_expr e ++
      pop t0 ++
      beq (reg t0) (reg zero) (lab ("wContinuar"^(string_of_int numLoop))) ++
      j (lab ("While"^(string_of_int numLoop))) ++
      (* Criar label para programa migrar para aqui, no final de if e else *)
      label ("wContinuar"^(string_of_int numLoop)) 

  | For (Set (id,e1) as atrib ,e2,e3,sFor) as instr -> begin
      forCounter := !forCounter + 1;
      let numLoop = !forCounter in
      (* Verificar tipagem das componentes do For; caso haja conflito tipagem.ml levantará exceção *)
      let _ = Tipagem.stmt instr in
      (* Executar atribuição e obter valor de 'id' *)
      let codAtrib = compile_instr atrib in
      codAtrib ++
      compile_expr (Var id) ++
      pop t3 ++
      (* Executar expressão e2 *)
      compile_expr e2 ++
      pop t1 ++
      let vE1 = Tipagem.valor e1 in
      let vE2 = Tipagem.valor e2 in
      let op = match (vE1 < vE2) with
	      | true -> bgt
	      | false -> blt
	  in
      (* Verificar se expressão valor de 'id' é superior ao de 'e2'*)
      op (reg t3) (reg t1) (lab ("fContinuar"^(string_of_int numLoop))) ++
      (* Executar instrucao *)
      let codSFor = compile_instr sFor in

      (* Criar label para while *)
      label ("For"^(string_of_int numLoop)) ++
      codSFor ++
      (* Incrementar 'id' em 'e3' vezes (vId = vId + vE3); O facto de ser com Sum, 
      obriga a que as expressões tenham valor int *)
      compile_instr (Set(id,(Binop(Sum,(Var id),e3)))) ++
      pop t0 ++
        
      (* Refazer verificação entre 'id' e 'e2' *)
      compile_expr (Var id) ++
      pop t2 ++
      compile_expr e2 ++
      pop t1 ++
      op (reg t2) (reg t1) (lab ("fContinuar"^(string_of_int numLoop))) ++

      j (lab ("For"^(string_of_int numLoop))) ++

      (* Criar label para programa migrar para aqui, no final de for *)
      label ("fContinuar"^(string_of_int numLoop)) 
  end

  | OutInt e as instr ->
      (* Verificar tipagem da expressão 'e'; caso não seja do tipo int tipagem.ml levantará exceção *)
      let _ = Tipagem.stmt instr in
      compile_expr e ++
      li (reg v0) (imm 1) ++  
      pop a0 ++
      syscall ++
      (* Introdução de newline após print *)
      li (reg v0) (imm 4) ++  
      la (reg a0) (lab "newline") ++
      syscall

  | OutFloat e as instr ->
      (* Verificar tipagem da expressão 'e'; caso não seja do tipo float tipagem.ml levantará exceção *)
      let _ = Tipagem.stmt instr in
      compile_expr e ++
      li (reg v0) (imm 2) ++  
      fpop f12 ++
      syscall ++
      (* Introdução de newline após print *)
      li (reg v0) (imm 4) ++  
      la (reg a0) (lab "newline") ++
      syscall 

   | OutString e as instr ->
      (* Verificar tipagem da expressão 'e'; caso não seja do tipo string tipagem.ml levantará exceção *)
      let _ = Tipagem.stmt instr in
      compile_expr e ++
      li (reg v0) (imm 4) ++
      pop a0 ++
      syscall ++
      (* Introdução de newline após print *)
      li (reg v0) (imm 4) ++  
      la (reg a0) (lab "newline") ++
      syscall    

  | _ -> Nop


(* Compila o programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let code = List.map compile_instr p in
  let code = List.fold_right (++) code nop in
  let p =
    { 
      text =
      glabel "main" ++
        move (reg fp) (reg sp) ++ (* Adição nossa!!! *)
        addiu (reg sp) (reg sp) (imm (-(!frame_size))) ++ 
        sw (reg fp) (ind ~ofs:(!frame_size - 8) sp)++ (* $fp = ... *)
        code ++
        li (reg v0) (imm 10) ++
        syscall;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ mword [1] ++ l) genv
          (Hashtbl.fold (fun f v l -> label f ++ mfloat [v] ++ l) genf
          	(Hashtbl.fold (fun s v l -> label s ++ mnewline v ++ l) gens
          		(Hashtbl.fold (fun s v l -> label s ++ mspace v ++ l) genSpace
      				(label "newline" ++ mnewline "\n"))));
      (* Funções declaradas depois main *)
	  
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  MIPS.print_program fmt p;
  (*  "flush" do buffer para garantir que tudo fica escrito antes do fecho
       deste  *)
  fprintf fmt "@?";
  close_out f