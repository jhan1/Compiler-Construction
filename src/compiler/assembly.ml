(** This file contains type declarations and functions related to the compiler's
    representation of x86-64 assembly language. *)

open Batteries;;

(** Describes the registers of x86-64 that our code will use. *)
type register =
  | RAX
  | RSP
  | RDI
  | RSI
  | RDX
  | RCX
  | R8
  | R9
  | R10
  | R11
  | RBP
;;

(** Describes a memory address expression in x86-64 assembly. *)
type address =
  | AddrByRegister of register
  | AddrByRegisterOffset of register * int
  | AddrByLabel of string
;;

(** Describes the type of arguments in our x86-64 assembly representation.  We
    use this type somewhat loosely: not every argument is valid everywhere an
    argument type is written below, but capturing the precise syntax limitations
    of x86 would make our assembly language types a lot more complicated.

    Note that the string argument of ArgConstant is the textual representation
    of the constant to be emitted to the assembly file, such as "5" or
    "0xFFFFFFFFFFFFFFFE".
*)
type argument =
  | ArgConstant of string
  | ArgRegister of register
  | ArgMemory of address
  | ArgLabelOffset of string * int
;;

(** The type that represents single x86 instructions. *)
type instruction =
  | AsmAdd of argument * argument
  | AsmIMul of argument * argument
  | AsmIDiv of argument
  | AsmMov of argument * argument
  | AsmSub of argument * argument
  | AsmShl of argument * argument
  | AsmShr of argument * argument
  | AsmSal of argument * argument
  | AsmSar of argument * argument
  | AsmAnd of argument * argument
  | AsmOr of argument * argument
  | AsmXor of argument * argument
  | AsmNot of argument
  | AsmNeg of argument
  | AsmLabel of string
  | AsmFuncLabel of string
  | AsmCmp of argument * argument
  | AsmJmp of string
  | AsmJe of string
  | AsmJne of string
  | AsmJl of string
  | AsmJle of string
  | AsmJg of string
  | AsmJge of string
  | AsmPush of argument
  | AsmPop of argument
  | AsmCall of string
  | AsmSection of string
  | AsmAlign of string 
  | AsmDq of string 
  | AsmGlobal of string 
  | AsmExtern of string
  | AsmRep of string
  | AsmCqo 
  | AsmEmptyLine 
  | AsmRet
;;

(** A function which transforms an x86 register into a string suitable for
    writing into an assembly language file. *)
let code_of_register (register : register) : string =
  (* ignore register; failwith "TODO: code_of_register" TODO: delete this line *)
  match register with 
  | RAX -> "RAX"
  | RSP -> "RSP"
  | RDI -> "RDI"
  | RSI -> "RSI"
  | RDX -> "RDX"
  | RCX -> "RCX"
  | R8 -> "R8"
  | R9 -> "R9"
  | R10 -> "R10"
  | R11 -> "R11"
  | RBP -> "RBP"
;;
let code_of_address (address : address) : string =
  (* ignore address; failwith "TODO: code_of_address" TODO: delete this line *)
  match address with 
  | AddrByRegister(register) -> "[" ^ code_of_register register ^"]"
  | AddrByRegisterOffset(register, offset) ->
    if offset >= 0 then
      "[" ^ code_of_register register ^ "+" ^ string_of_int offset^ "]"
    else
      "[" ^ code_of_register register ^ string_of_int offset^ "]"
  | AddrByLabel(label) -> 
      "[" ^ label ^ "]" 
;;

(** A function which transforms an x86 argument into a string suitable for
    writing into an assembly language file. *)
let code_of_argument (argument : argument) : string =
  (* ignore argument; failwith "TODO: code_of_argument" TODO: delete this line *)
  match argument with 
  | ArgConstant(str) -> str
  | ArgRegister(register) -> code_of_register register
  | ArgMemory(address) -> code_of_address address
  | ArgLabelOffset(label, offset) -> label ^ 
    if offset >= 0 then " + " ^ string_of_int offset else string_of_int offset
;;

(** A function which transforms an x86 instruction into a string suitable for
    writing into an assembly language file.  For example, given the input
    AsmRet, an appropriate return value might be "  ret\n".
  *)
let code_of_instruction (instruction : instruction) : string =
  (* ignore instruction; failwith "TODO: code_of_instruction" TODO: delete this line *)
  let string_of_arguments (arg1, arg2) : string = 
    code_of_argument arg1 ^ ", " ^ code_of_argument arg2
  in
  match instruction with 
  | AsmAdd( arg1, arg2 ) -> "add " ^ string_of_arguments (arg1, arg2)
  | AsmIMul( arg1, arg2 ) -> "imul " ^ string_of_arguments (arg1, arg2)
  | AsmIDiv( arg1 ) -> "idiv " ^ code_of_argument arg1
  | AsmMov( arg1, arg2 ) -> "mov " ^ string_of_arguments (arg1, arg2)
  | AsmSub( arg1, arg2 ) -> "sub " ^ string_of_arguments (arg1, arg2)
  | AsmShl( arg1, arg2 ) -> "shl " ^ string_of_arguments (arg1, arg2)
  | AsmShr( arg1, arg2 ) -> "shr " ^ string_of_arguments (arg1, arg2)
  | AsmSal( arg1, arg2 ) -> "sal " ^ string_of_arguments (arg1, arg2)
  | AsmSar( arg1, arg2 ) -> "sar " ^ string_of_arguments (arg1, arg2)
  | AsmAnd( arg1, arg2 ) -> "and " ^ string_of_arguments (arg1, arg2)
  | AsmOr( arg1, arg2 ) -> "or " ^ string_of_arguments (arg1, arg2)
  | AsmXor( arg1, arg2 ) -> "xor " ^ string_of_arguments (arg1, arg2)
  | AsmNot( arg ) -> "not " ^ code_of_argument (arg)
  | AsmNeg( arg ) -> "neg " ^ code_of_argument (arg)
  | AsmFuncLabel( str )
  | AsmLabel( str ) -> str ^ ":" 
  | AsmCmp( arg1, arg2 ) -> "cmp " ^ string_of_arguments (arg1, arg2)
  | AsmJmp ( str ) -> "jmp " ^ str 
  | AsmJe ( str ) -> "je " ^ str 
  | AsmJne ( str ) -> "jne " ^ str 
  | AsmJl ( str ) -> "jl " ^ str 
  | AsmJle ( str ) -> "jl " ^ str 
  | AsmJg ( str ) -> "jg " ^ str 
  | AsmJge ( str ) -> "jge " ^ str 
  | AsmPush ( arg1 ) -> "push " ^ code_of_argument arg1
  | AsmPop ( arg1 ) -> "pop " ^ code_of_argument arg1
  | AsmCall ( str ) -> "call " ^ str
  | AsmSection ( str ) -> "section " ^ str
  | AsmAlign ( str ) -> "align " ^ str 
  | AsmDq ( str ) -> "dq " ^ str
  | AsmGlobal ( str ) -> "global " ^ str 
  | AsmExtern ( str ) -> "extern " ^ str
  | AsmRep ( str ) -> "rep " ^ str
  | AsmEmptyLine -> ""
  | AsmCqo -> "cqo"
  | AsmRet -> "ret"
;;

(** A function which transforms a list of x86 instructions into a string
    suitable for writing into an assembly language file. *)
let rec print_indent (n_indent : int) : string =
  if ( n_indent <= 0 ) then "" else "\t" ^ print_indent (n_indent - 1)
;;
let rec code_of_instruction_list (instruction_list : instruction list) (n_indent : int): string =
  (* ignore instruction_list; failwith "TODO: code_of_instruction_list" TODO: delete this line *)
  match instruction_list with 
  | [] -> ""
  | instruction :: rest -> 
    let n_indent' = (
    match instruction with
    | AsmAdd (_, _) 
    | AsmIMul (_, _) 
    | AsmIDiv (_)
    | AsmMov (_, _) 
    | AsmSub (_, _) 
    | AsmShl (_, _) 
    | AsmShr (_, _) 
    | AsmSal (_, _) 
    | AsmSar (_, _) 
    | AsmAnd (_, _) 
    | AsmOr (_, _) 
    | AsmXor (_, _) 
    | AsmNot _ 
    | AsmNeg _ 
    | AsmLabel _ 
    | AsmCmp (_, _) 
    | AsmJmp _ 
    | AsmJe _ 
    | AsmJne _ 
    | AsmJl _ 
    | AsmJle _
    | AsmJge _
    | AsmJg _ 
    | AsmPush _ 
    | AsmPop _ 
    | AsmCall _ 
    | AsmRep _
    | AsmCqo
    
    | AsmAlign _ 
    | AsmDq _ 
    | AsmGlobal _ 
    | AsmRet 
    | AsmExtern _ -> n_indent 
    | AsmEmptyLine -> n_indent - 1
    | AsmFuncLabel _ 
    | AsmSection _ -> n_indent + 1
    ) 
    in
    print_indent n_indent ^ 
    code_of_instruction instruction ^ "\n" ^ 
    code_of_instruction_list rest n_indent' 
;;
