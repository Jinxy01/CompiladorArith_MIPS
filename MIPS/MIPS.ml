
(* Biblioteca para produção de código MIPS *)

open Format

type register =  string

(* Hard-wired para 0 *)
let zero: register = "$zero" 

let v0: register = "$v0"

(* Registos reservados para prints *)
let a0: register = "$a0" 
let a1: register = "$a1" 
let f12: register = "$f12"

(* Data temporária - não preservada por subprogramas *)
let t0: register = "$t0"
let t1: register = "$t1"
let t2: register = "$t2" 
let t3: register = "$t3"

(* Registos para flutuantes - não preservado por subprogramas *)
let f0: register = "$f0"
let f4: register = "$f4" 
let f5: register = "$f5"
let f6: register = "$f6"
let f7: register = "$f7"

(* Apontador de área global *)
let gp: register = "$gp" 
(* Apontador de pilha *)
let sp: register = "$sp" 
(* Apontador de frame *)
let fp: register = "$fp"
(* Endereço de retorno *) 
let ra: register = "$ra" 

type label = string

type operand = formatter -> unit -> unit

(* fprintf fmt -> print no formato fmt, X86_64.mli linha 43 *)
let reg r = fun fmt () -> fprintf fmt "%s" r
let funcreg s = fun fmt () -> fprintf fmt "%S" s
(* Em MIPS não precisamos de $ para adicionar valor de inteiro a registo *)
let imm i = fun fmt () -> fprintf fmt "%i" i
let fimm f = fun fmt () -> fprintf fmt "%f" f

let ind ?(ofs=0) ?index ?(scale=1) r = fun fmt () -> match index with
  | None -> fprintf fmt "%d(%s)" ofs r (* Existe em MIPS *)
  | Some r1 -> fprintf fmt "%d(%s,%s,%d)" ofs r r1 scale (* Averiguar se existe *)

let lab (l: label) = fun fmt () -> fprintf fmt "%s" l

type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

type text = [`text ] asm
type data = [`data ] asm

let buf = Buffer.create 17
(*formatter_of_buffer b returns a new formatter writing to buffer b. At the end of pretty-printing, the formatter must 
be flushed using Format.pp_print_flush or Format.pp_print_newline, to print all the pending material into the buffer.*)
let fmt = formatter_of_buffer buf
let ins x =
  Buffer.add_char buf '\t';
  kfprintf (fun fmt ->
    fprintf fmt "\n";
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.clear buf;
    S s
  ) fmt x

let pr_list fmt pr = function
  | []      -> ()
  | [i]     -> pr fmt i
  | i :: ll -> pr fmt i; List.iter (fun i -> fprintf fmt ", %a" pr i) ll

let pr_ilist fmt l =
  pr_list fmt (fun fmt i -> fprintf fmt "%i" i) l

let pr_flist fmt l =
  pr_list fmt (fun fmt f -> fprintf fmt "%f" f) l

let pr_alist fmt l =
  pr_list fmt (fun fmt (a : label) -> fprintf fmt "%s" a) l



(* ------------------------------------------------------------------------------------------------------------- *)

let syscall = ins "syscall" (* Chamada ao sistema *)

									(* Inteiros *)

let li a imm = ins "li %a, %a" a () imm () (* Load immediate *)
let move a b = ins "move %a, %a" a () b () (* $a <- $b *)
let lw a adr = ins "lw %a, %a" a () adr () (* $a <- mem[adr] <=> $a = MEM[$a + offset]; advance_pc (4); *)
let sw a adr = ins "sw %a, %a" a () adr () (* mem[adr] <- $a; The contents of $a is stored at the specified address -> MEM[$b + offset] = $a; advance_pc (4);  *) 

									(* Aritmética *)

let addiu r a imm = ins "addiu %a, %a, %a" r () a () imm () (* Addi unsigned*)
let add r a b = ins "add %a, %a, %a" r () a () b () (* $r0 <- $r1 + $r2 *)
let sub r a b = ins "sub %a, %a, %a" r () a () b () (* $r = $a - $b; advance_pc (4);  *) 
let div r a b = ins "div %a, %a, %a" r () a () b () (* $r0 <- $r1 / $r2 *)
let mul r a b = ins "mul %a, %a, %a" r () a () b () (* $r0 <- $r1 * $r2 (sem overflow) *)

									(* Flutuantes *)

let la a adr = ins "la %a, %a" a () adr () (* $r0 <- adr *)
let fli a imm = ins "li.s %a, %a" a () imm ()
let fmove a b = ins "mov.s %a, %a" a () b () (* $a <- $b *)
let flw a adr = ins "l.s %a, %a" a () adr () (* $a <- mem[adr] <=> $a = MEM[$a + offset]; advance_pc (4); *)
let fsw a adr = ins "s.s %a, %a" a () adr () (* mem[adr] <- $a; The contents of $a is stored at the specified address -> MEM[$b + offset] = $a; advance_pc (4);  *) 

									(* Aritmética *)

let fadd r a b = ins "add.s %a, %a, %a" r () a () b () (* $r0 <- $r1 + $r2 *)
let fsub r a b = ins "sub.s %a, %a, %a" r () a () b () (* $r = $a - $b; advance_pc (4);  *) 
let fdiv r a b = ins "div.s %a, %a, %a" r () a () b () (* $r0 <- $r1 / $r2 *)
let fmul r a b = ins "mul.s %a, %a, %a" r () a () b () (* $r0 <- $r1 * $r2 (sem overflow) *)

									(* Lógica *)

let slt r a b = ins "slt %a, %a, %a" r () a () b () (* if $a < $b $r = 1; advance_pc (4), 0 otherwise; else $d = 0; advance_pc (4); *)
let sgt r a b = ins "slt %a, %a, %a" r () b () a () (* inversão da slt *)
let sle r a b = ins "sle %a, %a, %a" r () a () b () (* $r0 <- 1 se $r1 <= $r2, $r0 <- 0 senão *) 
let sge r a b = ins "sle %a, %a, %a" r () b () a () (* inversão da sle *)
let seq r a b = ins "seq %a, %a, %a" r () a () b () (* $r0 <- 1 se $r1 = $r2, $r0 <- 0 senão *)
let sne r a b = ins "sne %a, %a, %a" r () a () b () (* $r0 <- 1 se $r1 <> $r2, $r0 <- 0 senão *)
let mand r a b = ins "and %a, %a, %a" r () a () b () (* r = a & b *)
let mor r a b = ins "or %a, %a, %a" r () a () b () (* $r = $a | $b; advance_pc (4);  *)

                  (* Saltos *)

let beq a b label = ins "beq %a, %a, %a" a () b () label ()
let bne a b offset = ins "bne %a, %a, %a" a () b () offset () (* Branch on not equal: if $a != $a advance_pc (offset << 2)); else advance_pc (4) *)
let bgt a b label = ins "bgt %a, %a, %a" a () b () label ()
let blt a b label = ins "blt %a, %a, %a" a () b () label ()
let j label = ins "j %a" label () (* Jumps to the calculated address: PC = nPC; nPC = (PC & 0xf0000000) | (target << 2); salto para label *)
let jr a = ins "jr %a" a () (* Jump to the address contained in register $a: PC = nPC; nPC = $a ; salto para $a *)
let jal label = ins "jal %a" label ()(* Jumps to the calculated address and stores the return address in $31:
$31 = PC + 8 (or nPC + 4); PC = nPC; nPC = (PC & 0xf0000000) | (target << 2); salto para label, $ra <- $pc + 1 *)

(* ------------------------------------------------------------------------------------------------------------- *)

let (++) x y = Cat (x, y)
let lab (l: label) = fun fmt () -> fprintf fmt "%s" l

let swWithPar a adr = ins "sw %a, (%a)" a () adr ()
let lwWithPar a adr = ins "lw %a, (%a)" a () adr ()

let fswWithPar a adr = ins "s.s %a, (%a)" a () adr ()
let flwWithPar a adr = ins "l.s %a, (%a)" a () adr ()

let push r = 
  addiu (reg sp) (reg sp) (imm (-4)) ++     (* Alocar espaço, movendo apontador da pilha *)
  swWithPar (reg r) (reg sp)                  (* Carregar para pilha o conteudo de r *)

let pop r =
  lwWithPar (reg r) (reg sp)  ++            (* Carregar conteudo de pilha para registo r *)
  addiu (reg sp) (reg sp) (imm 4)         (* Ajustar apontador da pilha *)

let fpush r = 
  addiu (reg sp) (reg sp) (imm (-4)) ++     (* Alocar espaço, movendo apontador da pilha *)
  fswWithPar (reg r) (reg sp)                  (* Carregar para pilha o conteudo de r *)

let fpop r =
  flwWithPar (reg r) (reg sp)  ++            (* Carregar conteudo de pilha para registo r *)
  addiu (reg sp) (reg sp) (imm 4) 

(* Nao será necessário provavelmente*)
let nop = Nop

(* Usado para designar a 'main' por exemplo *)
let label (s : label) = S (s ^ ":\n")
(* Cabeçalho: .globl main\n main:, é o mais usado *)
let glabel (s: label) = S ("\t.globl\t" ^ s ^ "\n" ^ s ^ ":\n")
let dlabel (s : label) = S (s ^ "\n")
let ret = ins "ret"


(* Data types, uso de pr_ilist, em linha 128 *)

let mascii l = ins ".ascii %a" pr_ilist l
let masciiz l = ins ".asciiz %a" pr_ilist l (* com terminator NULL *)
let mbyte l = ins ".byte %a" pr_ilist l (* Byte (4 Bits) data type is used for single integers without any decimal places. It can also be used to store character. *)
let mhalfword l = ins ".halfword %a" pr_ilist l (* A Halfword consists of two bytes. It is used to store half of a word. Each four bit is at address divisible by 2 *)
let mword l = ins ".word %a" pr_ilist l (* In .word data is stored in the form of 32 Bits. Its double of half word. It can also be used to initialize an Array. You can 
also use positive or negative sign as its 32 bit *)
let mfloat l = ins ".float %a" pr_flist l
let mnewline s = ins ".asciiz %S" s (* com terminator NULL *)
let mspace i = ins ".space %i" i (* com terminator NULL *)
let string s = ins ".string %S" s


let inline s = S s

type program = {
  text : [ `text ] asm;
  data : [ `data ] asm;
}

let rec pr_asm fmt = function
  | Nop          -> ()
  | S s          -> fprintf fmt "%s" s
  | Cat (a1, a2) -> pr_asm fmt a1; pr_asm fmt a2

let print_program fmt p =
  fprintf fmt ".text\n";
  pr_asm fmt p.text;
  fprintf fmt ".data\n";
  pr_asm fmt p.data;
  pp_print_flush fmt ()

(* Acaba por nao ser usada, ver linha 177-fim de compile.mli *)
let print_in_file ~file p =
  let c = open_out file in
  let fmt = formatter_of_out_channel c in
  print_program fmt p;
  close_out c