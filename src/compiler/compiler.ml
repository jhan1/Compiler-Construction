(** This file contains the definition of the compiler: the tool that translates
    an AST in our language into assembly language. *)

open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;
open Wellformedness;;
open ClosureConversion;;

let string_of_twice_int (n : int) : string = 
  Int64.to_string (Int64.mul (Int64.of_int n) (Int64.of_int 2) );;

(* let heap_size = 1048576 * 8;; *)
(* let heap_size = 1048576 * 8;; *)
(* let heap_size = 12000;; *)
let global_heap_cursor = "heap_cursor";;
let global_start_of_stack = "start_of_stack";;
let global_end_of_stack = "end_of_stack";;
let global_start_of_heap = "start_of_heap";;
let global_end_of_heap = "end_of_heap";;


let global_bird_main = "bird_main";;
let global_stopWithError = "stopWithError";;
let global_printValue = "printValue";;
let global_gc = "gc";;


let check_arg_mem_use ( arg : argument ) : int = 
  match arg with
  | ArgConstant _ -> 0
  | ArgRegister _ -> 0
  | ArgLabelOffset _ -> 0
  | ArgMemory mem -> (
    match mem with
    | AddrByLabel _
    | AddrByRegister _ -> 0
    | AddrByRegisterOffset (_, offset) -> -offset )
;;
let rec calc_stack_mem (instruction_list: instruction list) : int = 
  match instruction_list with 
  | [] -> 0
  | instruction :: rest ->
    let mem_use : int = (
      match instruction with
      | AsmAdd (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmIMul (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmIDiv (arg1) -> (check_arg_mem_use arg1)
      | AsmMov (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmSub (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmShl (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmShr (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmSal (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmSar (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmAnd (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmOr (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmXor (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmNot arg1 -> check_arg_mem_use arg1
      | AsmNeg arg1 -> check_arg_mem_use arg1
      | AsmFuncLabel _
      | AsmLabel _ -> 0
      | AsmRep _ -> 0
      | AsmCmp (arg1, arg2) -> max (check_arg_mem_use arg1) (check_arg_mem_use arg2)
      | AsmJmp _ -> 0
      | AsmJe _ -> 0
      | AsmJne _ -> 0
      | AsmJl _ -> 0
      | AsmJge _ -> 0
      | AsmJle _ -> 0
      | AsmJg _ -> 0
      | AsmPush arg1 -> check_arg_mem_use arg1
      | AsmPop arg1 -> check_arg_mem_use arg1
      | AsmCall _ -> 0
      | AsmSection _
      | AsmAlign _
      | AsmDq _
      | AsmGlobal _
      | AsmExtern _
      | AsmEmptyLine
      | AsmCqo
      | AsmRet -> 0
    ) in
      max (mem_use) (calc_stack_mem rest)
;;
let const_false = "0x7FFFFFFFFFFFFFFF";;
let const_true = "0xFFFFFFFFFFFFFFFF";;
let const_diff = "0x8000000000000000";;
let const_closure = "0x8000000000000000";;

let check_reg_int ( ( ArgRegister reg1) , (ArgRegister reg2) ): instruction list =
  let label1 : string = fresh_name "Label" in
  let label2 : string = fresh_name "Label" in
  [
    AsmMov(ArgRegister(reg2), ArgRegister(reg1));
    AsmAnd(ArgRegister(reg2), ArgConstant("1"));
    AsmCmp(ArgRegister(reg2), ArgConstant("1"));
    AsmJe(label1);
    AsmJmp(label2);
    AsmLabel(label1);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("1"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;
    AsmLabel(label2);   
  ]
;;

let check_reg_none_zero ( ArgRegister reg1): instruction list =
  let label1 : string = fresh_name "Label" in
  let label2 : string = fresh_name "Label" in
  [
    AsmCmp(ArgRegister(reg1), ArgConstant("0"));
    AsmJe(label1);
    AsmJmp(label2);
    AsmLabel(label1);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("6"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;
    AsmLabel(label2);   
  ]
;;

let check_reg_bool ( ( ArgRegister reg1) , (ArgRegister reg2) ): instruction list =
  let label1 : string = fresh_name "Label" in
  let label2 : string = fresh_name "Label" in
  [
    AsmMov(ArgRegister(reg2), ArgRegister(reg1));
    AsmAnd(ArgRegister(reg2), ArgConstant("3"));
    AsmCmp(ArgRegister(reg2), ArgConstant("3"));
    AsmJne(label1);
    AsmJmp(label2);
    AsmLabel(label1);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("2"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;   
    AsmLabel(label2);   
  ]
;;

let check_reg_tuple ( ( ArgRegister reg1) , (ArgRegister reg2) ): instruction list =
  let label1 : string = fresh_name "Label" in
  let label2 : string = fresh_name "Label" in
  [
    AsmMov(ArgRegister(reg2), ArgRegister(reg1));
    AsmAnd(ArgRegister(reg2), ArgConstant("3"));
    AsmCmp(ArgRegister(reg2), ArgConstant("1"));
    AsmJne(label1);
    AsmJmp(label2);
    AsmLabel(label1);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("3"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;   
    AsmLabel(label2);   
  ] @
  let label3 : string = fresh_name "Label" in
  let label4 : string = fresh_name "Label" in
  [
    AsmMov(ArgRegister(reg2), ArgMemory(AddrByRegisterOffset(reg1, -1)));
    AsmShr(ArgRegister(reg2), ArgConstant("63"));
    AsmCmp(ArgRegister(reg2), ArgConstant("1"));
    AsmJe(label3);
    AsmJmp(label4);
    AsmLabel(label3);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("3"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;   
    AsmLabel(label4);   
  ]
;;

(* Value in reg1 is the index. Value in temp_var is the Bird memory addr of the size of the tuple *)
let check_tuple_index ( ( ArgRegister reg1) , temp_var, (ArgRegister reg2) ) : instruction list = 
  (* Check whether the index is non-negative *)
  let label1 : string = fresh_name "Label" in
  let label2 : string = fresh_name "Label" in
  [
    (* AsmMov(ArgRegister(reg2), ArgRegister(reg1)); *)
    AsmCmp(ArgRegister(reg1), ArgConstant("0"));
    AsmJl(label1);
    AsmJmp(label2);
    AsmLabel(label1);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("4"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;   
    AsmLabel(label2);   
  ]  @
  (* Check whether the index is strictly less than the size of tuple *)
  let label3 : string = fresh_name "Label" in
  let label4 : string = fresh_name "Label" in
  [
    AsmMov(ArgRegister(reg2), temp_var);
    AsmMov(ArgRegister(reg2), ArgMemory(AddrByRegisterOffset(reg2, -1)));
    AsmSal(ArgRegister(reg2), ArgConstant("1"));
    AsmCmp(ArgRegister(reg1), ArgRegister(reg2));
    AsmJge(label3);
    AsmJmp(label4);
    AsmLabel(label3);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("4"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;   
    AsmLabel(label4);
  ]
;;

let check_reg_closure ( ( ArgRegister reg1) , (ArgRegister reg2) ): instruction list =
  let label1 : string = fresh_name "Label" in
  let label2 : string = fresh_name "Label" in
  [
    AsmMov(ArgRegister(reg2), ArgRegister(reg1));
    AsmAnd(ArgRegister(reg2), ArgConstant("3"));
    AsmCmp(ArgRegister(reg2), ArgConstant("1"));
    AsmJne(label1);
    AsmJmp(label2);
    AsmLabel(label1);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("5"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;   
    AsmLabel(label2);   
  ]  @
  let label3 : string = fresh_name "Label" in
  let label4 : string = fresh_name "Label" in
  [
    AsmMov(ArgRegister(reg2), ArgMemory(AddrByRegisterOffset(reg1, -1)));
    AsmShr(ArgRegister(reg2), ArgConstant("63"));
    AsmCmp(ArgRegister(reg2), ArgConstant("1"));
    AsmJne(label3);
    AsmJmp(label4);
    AsmLabel(label3);

    AsmPush(ArgRegister(reg1));
    AsmMov(ArgRegister(RDI), ArgConstant("5"));
    AsmCall("stopWithError");
    AsmPop(ArgRegister(reg1)) ;   
    AsmLabel(label4);   
  ]
;;
let rec compile_expression (env : environment) (e : expr)
  : instruction list =
  match e with 
  | EInt(i) -> [AsmMov( ArgRegister(RAX) , ArgConstant(string_of_twice_int i) )]
  | EBool(b) -> (
    if ( b = true ) then
      [AsmMov( ArgRegister(RAX) , ArgConstant(const_true) )]
    else
      [AsmMov( ArgRegister(RAX) , ArgConstant(const_false) )]
  )
  | ETuple(exp_lst) -> 
    let tuple_size = get_lstSize exp_lst in 
    let temp_var, new_env = allocate_temp_variable env in 
    [AsmMov(ArgRegister(R11), ArgMemory(AddrByLabel(global_heap_cursor)));
    (* store the old global heap cursor *)
      (* AsmPush(ArgRegister(R11));  *)

      (* AsmMov(ArgMemory(AddrByRegister(R11))) *)

      AsmMov(ArgRegister(RAX), ArgConstant(string_of_int tuple_size));
      (* AsmMov(ArgMemory(AddrByRegister(R11)), ArgRegister(RAX)); *)
      
      AsmAdd(ArgRegister(RAX), ArgConstant("2"));
      AsmSal(ArgRegister(RAX), ArgConstant("3"));
      (* AsmAdd(ArgRegister(RAX), ArgRegister(R11)); *)
      (* Check whether we need to call gc *)
      AsmMov(ArgRegister(R10), ArgMemory(AddrByLabel(global_end_of_heap)));
      AsmMov(ArgRegister(R9), ArgRegister(R11));
      AsmAdd(ArgRegister(R9), ArgRegister(RAX));
      AsmCmp(ArgRegister(R9), ArgRegister(R10));
    ] @
    let label1 : string = fresh_name "Label" in
    [
      AsmJle(label1);
      (* Set end_of_stack  *)
      
      (* AsmAdd(ArgRegister(R11), ArgConstant("1"));
      AsmMov(temp_var, ArgRegister(R11));
      AsmSub(ArgRegister(R11), ArgConstant("1")); *)
      
      AsmPush(ArgRegister(RAX));
      (* AsmPush(ArgRegister(R11)); *)
      (* AsmMov(ArgRegister(RDI), ArgRegister(R9));
      AsmSub(ArgRegister(RDI), ArgRegister(R10)); *)
      AsmMov(ArgRegister(RDI), ArgRegister(RAX));
      AsmMov(ArgMemory(AddrByLabel(global_end_of_stack)), ArgRegister(RSP));
      AsmCall("gc");
      (* AsmPop(ArgRegister(R11)); *)
      AsmPop(ArgRegister(RAX));

      AsmLabel(label1);


      (* update global heap cursor *)
      AsmMov(ArgRegister(R11), ArgMemory(AddrByLabel(global_heap_cursor)));
    
      AsmMov(ArgRegister(R9), ArgConstant(string_of_int tuple_size));
      AsmMov(ArgMemory(AddrByRegister(R11)), ArgRegister(R9));

      AsmAdd(ArgRegister(R11), ArgConstant("1"));
      AsmMov(temp_var, ArgRegister(R11));
      AsmSub(ArgRegister(R11), ArgConstant("1"));     
      AsmAdd(ArgRegister(RAX), ArgRegister(R11));
      AsmMov(ArgMemory(AddrByLabel(global_heap_cursor)), ArgRegister(RAX));
      AsmAdd(ArgRegister(R11), ArgConstant("8"));
      


      ] @
    let rec mov_expr (exp_lst: expr list) : instruction list = 
      match exp_lst with
      | [] -> []
      | exp :: rest -> 
        [AsmPush(ArgRegister(R11))] @
        compile_expression new_env exp @ 
        [AsmPop(ArgRegister(R11));
        AsmAdd(ArgRegister(R11), ArgConstant("8"));
          AsmMov(ArgMemory(AddrByRegister(R11)), ArgRegister(RAX)); 
        ] @
        mov_expr rest
    in
    mov_expr exp_lst @ 
    [
      (* AsmPop(ArgRegister(RAX)); *)
    AsmMov(ArgRegister(RAX), temp_var);
    AsmXor(ArgRegister(R11), ArgRegister(R11));
    AsmMov(temp_var, ArgRegister(R11));
    (* AsmAdd(ArgRegister(RAX), ArgConstant("1")); *)
    ]

  | EVar(key) -> [AsmMov( ArgRegister(RAX) , (find_named_variable key env) )]

  | EUnaryOp(operator, expr_1) -> (
    match operator with
    | OpAfter -> (compile_expression env expr_1) @ (check_reg_int (ArgRegister(RAX), ArgRegister(R11))) @ [AsmAdd( ArgRegister(RAX), ArgConstant("2"))]
    | OpBefore -> (compile_expression env expr_1) @ (check_reg_int (ArgRegister(RAX), ArgRegister(R11))) @ [AsmSub( ArgRegister(RAX), ArgConstant("2"))]
    | OpIsBool -> (
      (compile_expression env expr_1 ) @
      AsmMov( ArgRegister(R10), ArgRegister(RAX)) ::
      AsmSar( ArgRegister(R10), ArgConstant("1")) ::
      AsmAnd( ArgRegister(RAX), ArgRegister(R10)) ::
      AsmSal( ArgRegister(RAX), ArgConstant("63")) ::
      AsmMov( ArgRegister(R10), ArgConstant(const_false) ) ::
      [AsmOr( ArgRegister(RAX), ArgRegister(R10) )]
    )
    
    | OpIsInt -> 
      (compile_expression env expr_1 ) @
      AsmSal( ArgRegister(RAX), ArgConstant("63")) ::
      [AsmNot( ArgRegister(RAX)) ]
    
    | OpIsTuple -> (
      (compile_expression env expr_1 ) @
      (* check whether the last two bits are 0 and 1 *)
      AsmMov( ArgRegister(R10), ArgRegister(RAX)) ::
      AsmMov( ArgRegister(R11), ArgRegister(RAX)) ::
      AsmSar( ArgRegister(R10), ArgConstant("1")) ::
      AsmXor( ArgRegister(R10), ArgConstant("1")) ::
      AsmAnd( ArgRegister(RAX), ArgRegister(R10)) ::
      AsmAnd( ArgRegister(RAX), ArgConstant("1")) ::
      (* check whether the leading bit is 0 *)
      let label1 : string = fresh_name "Label" in
      let label2 : string = fresh_name "Label" in
      [
        AsmCmp( ArgRegister(RAX), ArgConstant("1"));
        AsmJe( label1 );
        AsmMov( ArgRegister(RAX), ArgConstant("0"));
        AsmJmp( label2);
        AsmLabel( label1);
        AsmMov( ArgRegister(R11), ArgMemory(AddrByRegisterOffset(R11, -1)) );
        AsmNot( ArgRegister(R11 ));
        AsmShr( ArgRegister(R11), ArgConstant("63") );
        AsmAnd( ArgRegister(RAX), ArgRegister(R11));
        AsmLabel( label2);  
      ] @
      AsmSal( ArgRegister(RAX), ArgConstant("63")) ::

      AsmMov( ArgRegister(R10), ArgConstant(const_false) ) ::
      [AsmOr( ArgRegister(RAX), ArgRegister(R10) )]
    )

    | OpPrint -> 
      (compile_expression env expr_1 ) @
      AsmPush(ArgRegister(RAX)) ::
      AsmMov(ArgRegister(RDI), ArgRegister(RAX)) ::
      AsmCall("printValue") ::
      [AsmPop(ArgRegister(RAX))]
  )

  | EBinaryOp(operator , expr_1, expr_2) -> (
    let temp_var, new_env = allocate_temp_variable env in 
    (compile_expression env expr_1) @ (
    match operator with
    | OpPlus 
    | OpMinus 
    | OpTimes 
    | OpDiv
    | OpMod
    | OpLessThan 
    | OpGreaterThan ->
      check_reg_int (ArgRegister(RAX), ArgRegister(R11))
    | OpEqualTo -> []
    | OpAnd 
    | OpOr -> 
      check_reg_bool (ArgRegister(RAX), ArgRegister(R11))
    | OpTupleIndex ->
      check_reg_tuple (ArgRegister(RAX), ArgRegister(R11))
    ) @ 
    [AsmMov( temp_var, ArgRegister(RAX) )] @ 
    (compile_expression new_env expr_2) @ (
    match operator with
    | OpPlus -> check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @ [AsmAdd( ArgRegister(RAX), temp_var)]
    | OpMinus -> check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @ [AsmSub( ArgRegister(RAX), temp_var)] @ [AsmNeg(ArgRegister(RAX))]
    | OpTimes -> 
      check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @ 
      [AsmSar( ArgRegister(RAX), ArgConstant("1")) ; 
      
      AsmIMul( ArgRegister(RAX), temp_var) ]
    | OpDiv -> 
      check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @
      check_reg_none_zero (ArgRegister(RAX)) @
      [
        AsmMov( ArgRegister(R11), ArgRegister(RAX) );
        AsmMov( ArgRegister(RAX), temp_var);
        (* AsmXor( ArgRegister(RDX), ArgRegister(RDX)); *)
        AsmCqo;
        AsmIDiv( ArgRegister(R11) );
        AsmSal( ArgRegister(RAX), ArgConstant("1"));
      ]

    | OpMod -> 
      check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @
      check_reg_none_zero (ArgRegister(RAX)) @
      [
        AsmMov( ArgRegister(R11), ArgRegister(RAX) );
        AsmMov( ArgRegister(RAX), temp_var);
        (* AsmXor( ArgRegister(RDX), ArgRegister(RDX)); *)
        AsmCqo;
        AsmIDiv( ArgRegister(R11) );
        AsmMov( ArgRegister(RAX), ArgRegister(RDX));
        (* AsmSal( ArgRegister(RAX), ArgConstant("1")); *)
      ]

    | OpGreaterThan -> 
      check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @ 
      let label_l1 : string = fresh_name "Label" in
      let label_l2 : string = fresh_name "Label" in
      [ 
        AsmCmp( ArgRegister(RAX), temp_var) ;
        AsmJl( label_l1 ) ;
        AsmMov( ArgRegister(RAX), ArgConstant(const_false)) ;
        AsmJmp( label_l2 ) ;
        AsmLabel(label_l1) ;
        AsmMov( ArgRegister(RAX), ArgConstant(const_true)) ;
        AsmLabel(label_l2)   
      ]

    | OpLessThan -> 
      check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @ 
      let label_g1 : string = fresh_name "Label" in
      let label_g2 : string = fresh_name "Label" in
      [ 
        AsmCmp( ArgRegister(RAX), temp_var) ;
        AsmJg( label_g1 ) ;
        AsmMov( ArgRegister(RAX), ArgConstant(const_false)) ;
        AsmJmp( label_g2 ) ;
        AsmLabel(label_g1) ;
        AsmMov( ArgRegister(RAX), ArgConstant(const_true)) ;
        AsmLabel(label_g2)   
      ]

    | OpEqualTo -> 
      let label_eq1 : string = fresh_name "Label" in
      let label_eq2 : string = fresh_name "Label" in
      [ 
        AsmCmp( ArgRegister(RAX), temp_var) ;
        AsmJe( label_eq1 ) ;
        AsmMov( ArgRegister(RAX), ArgConstant(const_false)) ;
        AsmJmp( label_eq2 ) ;
        AsmLabel(label_eq1) ;
        AsmMov( ArgRegister(RAX), ArgConstant(const_true)) ;
        AsmLabel(label_eq2)   
      ]
    
    | OpAnd -> 
      check_reg_bool (ArgRegister(RAX), ArgRegister(R11)) @ [ AsmAnd( ArgRegister(RAX), temp_var )]
    | OpOr -> 
      check_reg_bool (ArgRegister(RAX), ArgRegister(R11)) @ [ AsmOr( ArgRegister(RAX), temp_var )]
    
    | OpTupleIndex -> 
      check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @
      check_tuple_index (ArgRegister(RAX), temp_var, ArgRegister(R11)) @
      [
      AsmAdd(ArgRegister(RAX), ArgConstant("4"));
      AsmSal(ArgRegister(RAX), ArgConstant("2"));
      AsmMov(ArgRegister(R11), temp_var);
      AsmAdd(ArgRegister(RAX), ArgRegister(R11));
      AsmXor(ArgRegister(RAX), ArgConstant("1"));
      AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(RAX)))
      ]
    ) @ 
    [
      AsmMov( ArgRegister(R9), ArgConstant("0"));
      AsmMov( temp_var, ArgRegister(R9));
    ]
    
  )

  | ELet(key , expr_1, expr_2) -> 
      
      let new_env = (allocate_named_variable key env) in 
      (compile_expression env expr_1) @ 
      [AsmMov( (find_named_variable key new_env) , ArgRegister(RAX) )] @
      (compile_expression new_env expr_2) @
      [ AsmXor( ArgRegister(R9), ArgRegister(R9));
      AsmMov( (find_named_variable key new_env) , ArgRegister(R9) )]

  
  | EIf(cond, expr_1, expr_2) -> (
    (compile_expression env cond) @
    check_reg_bool (ArgRegister(RAX), ArgRegister(R11)) @ 
    AsmMov( ArgRegister(R10), ArgConstant(const_true)) ::
    AsmCmp( ArgRegister(RAX), ArgRegister(R10)) ::
    let label1 : string = fresh_name "Label" in
    let label2 : string = fresh_name "Label" in
      AsmJne( label1 ) ::
      (compile_expression env expr_1) @
      AsmJmp( label2 ) ::
      AsmLabel(label1) ::
      (compile_expression env expr_2) @
      [AsmLabel(label2)]
    )
    
  | EAppl( expr1, expr2, isTail ) -> 
      compile_expression env expr1 @
      check_reg_closure (ArgRegister(RAX), ArgRegister(R11)) @ 
      let temp_var, new_env = (allocate_temp_variable env) in
      [AsmMov(temp_var, ArgRegister(RAX))] @
      compile_expression new_env expr2 @
      let add_arg : string = fresh_name "Label" in
      let call_fun : string = fresh_name "Label" in
      let temp_var', _new_env' = (allocate_temp_variable new_env) in
      [
        (* Compare the number of arguments in the closure to the number of parameters for the function *)
        AsmMov(ArgRegister(R11), ArgRegister(RAX)); (* R11 stores the new argument *)
        AsmMov(ArgRegister(RAX), temp_var);
        (* AsmXor( ArgRegister(R9), ArgRegister(R9));
        AsmMov(temp_var, ArgRegister(R9)); *)
        AsmMov(ArgRegister(R9), ArgMemory(AddrByRegisterOffset(RAX, -1))); (* # of arguments*)
        AsmMov(ArgRegister(R10), ArgConstant(const_closure));
        AsmSub(ArgRegister(R9), ArgRegister(R10));
        AsmAdd(ArgRegister(R9), ArgConstant("1"));
        AsmMov(ArgRegister(R10), ArgMemory(AddrByRegisterOffset(RAX, 15))); (* # of params*)
        AsmCmp(ArgRegister(R9), ArgRegister(R10));

        
        AsmJl(add_arg);
      ]
      @
      (if isTail then
      let non_tail_call = fresh_name "non_tail_call" in
      let n_params = find_named_variable name_num_params env in
      [
        (* Call function When is a tail call*)
        AsmMov(ArgRegister(R9), n_params);
        AsmCmp(ArgRegister(R10), ArgRegister(R9));
        AsmJg(non_tail_call);
        (* If satisfies the condition *)
        
        AsmMov(ArgRegister(RCX), ArgRegister(R10));
        AsmSub(ArgRegister(RCX), ArgConstant("1"));
        (* Move the last argument directly to stack *)
        (* AsmMov(ArgMemory(AddrByRegisterOffset(RSP, -8)), ArgRegister(R11)); *)
        (* AsmSub(ArgRegister(RSP), ArgConstant("8")); *)
        AsmMov(ArgRegister(RDI), ArgRegister(RBP));
        AsmAdd(ArgRegister(RDI), ArgConstant("16"));
        AsmMov(ArgRegister(RSI), ArgRegister(RAX));
        AsmAdd(ArgRegister(RSI), ArgConstant("31"));
  
        
        (* AsmAdd(ArgRegister(RSP), ArgConstant("8")); *)
        (* Add RSP *)
        AsmAdd(ArgRegister(R10), ArgConstant("1"));
        AsmSal(ArgRegister(R10), ArgConstant("3"));
        AsmMov(ArgRegister(R9), ArgRegister(RBP));
        AsmAdd(ArgRegister(R9), ArgRegister(R10));
        AsmMov(ArgMemory(AddrByRegister(R9)), ArgRegister(R11));
        (* AsmSub (ArgRegister(RSP), ArgRegister(R10)); *)
        (* Store R10 *)
        (* AsmMov (temp_var', ArgRegister(R10)); *)
        AsmPush(ArgRegister(RAX));
        AsmRep("movsq");
        AsmPop(ArgRegister(RAX));
        (* Tear down TCr stack *)
        AsmMov(ArgRegister(RSP), ArgRegister(RBP));
        AsmPop(ArgRegister(RBP));

        AsmAdd(ArgRegister(RAX), ArgConstant("23"));
        AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(RAX)));
        (* AsmCall("RAX"); *)
        AsmJmp("RAX");
        (* AsmCall("fun_prod"); *)
        (* AsmMov(ArgRegister(R10), temp_var');
        AsmAdd(ArgRegister(RSP), ArgRegister(R10));         *)
        
        

        
        
        
        
        
        
        
        AsmJmp(call_fun);
        AsmLabel(non_tail_call);
        (* Set RCX *)
        AsmMov(ArgRegister(RCX), ArgRegister(R10));
        AsmSub(ArgRegister(RCX), ArgConstant("1"));
        (* Move the last argument directly to stack *)
        (* AsmMov(ArgMemory(AddrByRegisterOffset(RSP, -8)), ArgRegister(R11)); *)
        AsmSub(ArgRegister(RSP), ArgConstant("8"));
        AsmMov(ArgMemory(AddrByRegister(RSP)), ArgRegister(R11));
        AsmAdd(ArgRegister(RSP), ArgConstant("8"));
        (* Add RSP *)
        AsmSal(ArgRegister(R10), ArgConstant("3"));
        AsmSub (ArgRegister(RSP), ArgRegister(R10));
        (* Store R10 *)
        AsmMov (temp_var', ArgRegister(R10));

        AsmMov(ArgRegister(RSI), ArgRegister(RAX));
        AsmAdd(ArgRegister(RSI), ArgConstant("31"));
        AsmMov(ArgRegister(RDI), ArgRegister(RSP));
        AsmPush(ArgRegister(RAX));
        AsmRep("movsq");
        AsmPop(ArgRegister(RAX));
        AsmAdd(ArgRegister(RAX), ArgConstant("23"));
        AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(RAX)));
        AsmCall("RAX");
        (* AsmCall("fun_prod"); *)
        AsmMov(ArgRegister(R10), temp_var');
        AsmAdd(ArgRegister(RSP), ArgRegister(R10));

        AsmJmp(call_fun);
      ] else 
      [
        (* Call function when is not a tail call*)
        (* Set RCX *)
        AsmMov(ArgRegister(RCX), ArgRegister(R10));
        AsmSub(ArgRegister(RCX), ArgConstant("1"));
        (* Move the last argument directly to stack *)
        (* AsmMov(ArgMemory(AddrByRegisterOffset(RSP, -8)), ArgRegister(R11)); *)
        AsmSub(ArgRegister(RSP), ArgConstant("8"));
        AsmMov(ArgMemory(AddrByRegister(RSP)), ArgRegister(R11));
        AsmAdd(ArgRegister(RSP), ArgConstant("8"));
        (* Add RSP *)
        AsmSal(ArgRegister(R10), ArgConstant("3"));
        AsmSub (ArgRegister(RSP), ArgRegister(R10));
        (* Store R10 *)
        AsmMov (temp_var', ArgRegister(R10));

        AsmMov(ArgRegister(RSI), ArgRegister(RAX));
        AsmAdd(ArgRegister(RSI), ArgConstant("31"));
        AsmMov(ArgRegister(RDI), ArgRegister(RSP));
        AsmPush(ArgRegister(RAX));
        AsmRep("movsq");
        AsmPop(ArgRegister(RAX));
        AsmAdd(ArgRegister(RAX), ArgConstant("23"));
        AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegister(RAX)));
        AsmCall("RAX");
        (* AsmCall("fun_prod"); *)
        AsmMov(ArgRegister(R10), temp_var');
        AsmAdd(ArgRegister(RSP), ArgRegister(R10));

        AsmJmp(call_fun);
      ])
      @
      [

        AsmLabel(add_arg);
        
        (* Add argument *)
        AsmMov(ArgRegister(RSI), ArgRegister(RAX));
        
        AsmMov(ArgRegister(R8), ArgMemory(AddrByLabel(global_heap_cursor)));
        (* AsmMov(ArgRegister(R10)) *)
        (* AsmAdd(ArgRegister(R8), ArgConstant("1"));
        AsmMov(temp_var', ArgRegister(R8));
        AsmSub(ArgRegister(R8), ArgConstant("1")); *)
        (* Check whether we need to call gc *)
        AsmMov(ArgRegister(RAX), ArgMemory(AddrByLabel(global_end_of_heap)));
        AsmAdd(ArgRegister(R9), ArgConstant("3"));
        AsmSal(ArgRegister(R9), ArgConstant("3"));
        AsmSub(ArgRegister(RAX), ArgRegister(R9));
        AsmCmp(ArgRegister(R8), ArgRegister(RAX));

      ] @
      let label1 : string = fresh_name "Label" in
      [
        AsmJl(label1);

        
        (* AsmAdd(ArgRegister(R8), ArgConstant("1")); *)
        (* AsmPush(ArgRegister(R10));
        AsmPush(ArgRegister(R8));
         *)
        AsmPush(ArgRegister(R9));
        AsmPush(ArgRegister(R11));
        AsmPush(ArgRegister(RSI)); 
        AsmXor(ArgRegister(RSI), ArgRegister(RSI));      
        AsmMov(ArgMemory(AddrByLabel(global_end_of_stack)), ArgRegister(RSP));
        
        (* AsmSub(ArgRegister(R8), ArgRegister(RAX));
        AsmMov(ArgRegister(RDI), ArgRegister(R8)); *)
        AsmMov(ArgRegister(RDI), ArgRegister(R9));
        AsmCall("gc");
        AsmPop(ArgRegister(RSI));
        AsmPop(ArgRegister(R11));
        AsmPop(ArgRegister(R9));
        (* AsmPop(ArgRegister(R8));
        AsmPop(ArgRegister(R10)); *)

        (* AsmSub(ArgRegister(R8), ArgConstant("1")); *)


        AsmLabel(label1);

        AsmSub(ArgRegister(RSI), ArgConstant("1"));
        AsmMov(ArgRegister(R8), ArgMemory(AddrByLabel(global_heap_cursor)) );
        AsmSar(ArgRegister(R9), ArgConstant("3"));
        (* AsmSub(ArgRegister(R9), ArgConstant("3")); *)

        AsmMov(ArgRegister(RDI), ArgRegister(R8));
        AsmMov(ArgRegister(RCX), ArgRegister(R9));
        (* AsmAdd(ArgRegister(RCX), ArgConstant("3")); *)
        AsmPush(ArgRegister(R8));
        AsmPush(ArgRegister(R9));
        AsmPush(ArgRegister(R11));
        AsmRep("movsq");
        AsmPop(ArgRegister(R11));
        AsmPop(ArgRegister(R9));
        AsmPop(ArgRegister(R8));
        (* AsmMov(ArgMemory(AddrByRegister(R8)), ArgRegister(R9) );
        AsmMov(ArgRegister(R10), ArgConstant(const_closure)); *)
        (* Update the # of arguments (with the top bit 1) *)
        AsmMov(ArgRegister(R10), ArgMemory(AddrByRegister(R8)));
        AsmMov(ArgRegister(RAX), ArgConstant("1"));
        AsmAdd(ArgRegister(R10), ArgRegister(RAX));
        AsmMov(ArgMemory(AddrByRegister(R8)), ArgRegister(R10));
        (* Update RAX *)
        AsmMov(ArgRegister(RAX), ArgRegister(R8));
        AsmAdd(ArgRegister(RAX), ArgConstant("1"));
        (* AsmMov(ArgRegister(RAX), temp_var'); *)
        (* AsmMov(ArgRegister()) *)
        (* AsmAdd(ArgRegister(R9), ArgConstant("3")); *)
        AsmSal(ArgRegister(R9), ArgConstant("3"));
        AsmAdd(ArgRegister(R8), ArgRegister(R9));
        AsmMov(ArgMemory(AddrByRegister(R8)), ArgRegister(R11));
        AsmAdd(ArgRegister(R8), ArgConstant("8"));
        AsmMov(ArgMemory(AddrByLabel(global_heap_cursor)), ArgRegister(R8));
        AsmLabel(call_fun);


        AsmXor( ArgRegister(R9), ArgRegister(R9));
        AsmMov( temp_var, ArgRegister(R9));
        AsmMov( temp_var', ArgRegister(R9));
      ]

  | ELambda (_str, _expr) -> []

  | ESet( expr1, expr2, expr3 ) ->
      compile_expression env expr1 @
      check_reg_tuple (ArgRegister(RAX), ArgRegister(R11)) @
      let temp_var, new_env = allocate_temp_variable env in 
      [ AsmMov(temp_var, ArgRegister(RAX)) ] @
      compile_expression new_env expr2 @
      check_reg_int (ArgRegister(RAX), ArgRegister(R11)) @
      check_tuple_index (ArgRegister(RAX), temp_var, ArgRegister(R11)) @
      let temp_var', new_env' = allocate_temp_variable new_env in 
      [ AsmMov(temp_var', ArgRegister(RAX)) ] @
      compile_expression new_env' expr3 @ 
      [
        AsmMov(ArgRegister(R9), temp_var);
        AsmMov(ArgRegister(R10), temp_var');
        AsmAdd(ArgRegister(R10), ArgConstant("4"));
        AsmSal(ArgRegister(R10), ArgConstant("2"));
        AsmAdd(ArgRegister(R9), ArgRegister(R10));
        AsmMov(ArgMemory(AddrByRegisterOffset(R9, -1)), ArgRegister(RAX));

        AsmXor( ArgRegister(R9), ArgRegister(R9));
        AsmMov( temp_var, ArgRegister(R9));
        AsmMov( temp_var', ArgRegister(R9));
      ]




  (* |ECall( func_name , exp_lst) ->
      (* [AsmPush (ArgRegister(RAX))] @ *)

      let rec push_params (env : environment) (exp_lst : expr list)  : instruction list = (
        match exp_lst with
        | [] -> []
        | e :: rest ->
          compile_expression env e @ 
          let temp_var, new_env = allocate_temp_variable env in 
          [AsmMov(temp_var, ArgRegister(RAX))] @
          push_params new_env rest @ [AsmMov(ArgRegister(RAX), temp_var); AsmPush(ArgRegister(RAX))] 
      ) 
      
      in 
      push_params env exp_lst @ 
      [AsmCall("fn_" ^ func_name)] @
      [AsmMov(ArgRegister(R11) , ArgRegister(RAX))] @
      let rec pop_params (env : environment) (exp_lst : expr list)  : instruction list = (
        match exp_lst with
        | [] -> []
        | _ :: rest ->
          [AsmPop(ArgRegister(RAX))] @ pop_params env rest 
      ) 
      in 
      pop_params env exp_lst @
      [
      (* AsmPop(ArgRegister(RAX)) ; *)

      AsmMov(ArgRegister(RAX), ArgRegister(R11))
      ] *)


;;

let rec mark_tail (e : expr) : expr =
  match (e : expr) with
  | EInt _ -> e
  | EBool _ -> e
  | EUnaryOp (_op, _exp) -> e 
    (* let e' = mark_tail exp in 
    EUnaryOp (op, e') *)

  | EBinaryOp (_op, _exp1, _exp2) -> e
    (* let e1' = mark_tail exp1 in 
    let e2' = mark_tail exp2 in
    EBinaryOp (op, e1', e2') *)

  | ETuple _exp_lst -> e
    (* let rec check_exp_lst (expr_lst: expr list): expr list = 
      match expr_lst with
      | [] -> []
      | e :: rest -> 
        let new_e = mark_tail e in
        let rest_e_lst = check_exp_lst rest in 
        new_e :: rest_e_lst in
    let new_expr_lst = check_exp_lst exp_lst in 
    ETuple(new_expr_lst) *)

  | ELet (key, e1, e2) -> 
    let new_e2 = mark_tail e2 in 
    ELet (key, e1, new_e2)

  | EVar _ -> e

  | EIf (e1, e2, e3) -> 
    let new_e2 = mark_tail e2 in 
    let new_e3 = mark_tail e3 in 
    EIf(e1, new_e2, new_e3)

  | EAppl (e1, e2, _) ->
    (* let _ = print_endline "set the apple to true" in   *)
    EAppl( e1, e2, true)

  | ESet (_, _, _) -> e

  | ELambda (_, _) -> failwith "impossible"
;; 

let mark_tail_program (prog: program) : program = 
  match (prog : program) with 
  | Program (dec_lst, e) -> 
    let rec mark_tail_dec_lst (dec_lst : declaration list) : declaration list = 
      match dec_lst with
      | [] -> []
      | dec :: rest ->
        match dec with 
        | DFunction (func_name, params, e) -> 
          let new_e = mark_tail e in 
          DFunction( func_name, params, new_e) :: mark_tail_dec_lst rest
    in
    Program( mark_tail_dec_lst dec_lst, e)
;;

let rec move_params (param_list : string list) (offset : int) : instruction list = 
  match param_list with
  | [] -> []
  | _ :: rest ->
    [AsmMov(ArgRegister(RAX), ArgMemory(AddrByRegisterOffset(RBP, offset + 8)))] @
    AsmMov(ArgMemory(AddrByRegisterOffset(RBP, -offset)), ArgRegister(RAX)) :: (move_params rest (offset + 8))
  ;;

let compile_declaration (dec : declaration) (env : environment): instruction list=
  match dec with
  | DFunction(key, param_list, e) -> 
    let new_env = get_new_env_from_param_list param_list env in
    let new_env' = set_n_params (get_lstSize param_list) new_env in
    let instructions = compile_expression new_env' e in
    let stack_mem = calc_stack_mem instructions in
    [AsmFuncLabel(get_func_name key) ;
    AsmPush(ArgRegister(RBP)) ;
    AsmMov( ArgRegister(RBP), ArgRegister(RSP)) ;
    AsmSub(ArgRegister(RSP), ArgConstant(string_of_int (stack_mem)))] @ 
    

    [AsmXor( ArgRegister(RAX), ArgRegister(RAX) );
    AsmMov( ArgRegister(RCX), ArgConstant(string_of_int stack_mem) );
    AsmSar( ArgRegister(RCX), ArgConstant("3") );
    AsmMov( ArgRegister(RDI), ArgRegister(RSP));
    AsmRep("stosq");] @

    move_params param_list 8 @

    instructions @
    [AsmMov(ArgRegister(RSP), ArgRegister(RBP));
    AsmPop(ArgRegister(RBP));
    AsmRet;
    AsmEmptyLine]
;;


let rec compile_dec_list (lst : declaration list) (env : environment) : instruction list = 
  match lst with
  | [] -> []
  | dec::rest ->
    (* let label : string = fresh_name "Label" in *)
    (* AsmJmp(label) :: *)
    compile_declaration dec env @ 
    (* AsmLabel(label) :: *)
    compile_dec_list rest env 
;;


let rec compile_base_cases (dec_list : declaration list) : instruction list = 
  match dec_list with 
  | [] -> []
  | dec :: rest -> (
    match dec with 
    | DFunction(func, param_list, _) ->
    let init_closure = const_closure ^ ", 0, " ^ string_of_int (get_lstSize param_list) ^ ", " ^ get_func_name func ^ "" in
    [
      AsmAlign("8");
      AsmFuncLabel(get_closure_name func);
      AsmDq(init_closure);
      AsmEmptyLine;
    ] @ 
    compile_base_cases rest
  )
;;
let compile_program (prog : program) : instruction list =
  match prog with
  | Program(dec_lst , e) ->  
  let init_environment = add_init_environment dec_lst in 
  let declarations = compile_dec_list dec_lst init_environment in 
  let base_closures = compile_base_cases dec_lst in 
  let instructions = compile_expression init_environment e in

  (* let () = print_endline (string_of_int (calc_stack_mem instructions)) in   *)
  let stack_mem = calc_stack_mem instructions in
  [
    AsmSection(".data");
    AsmAlign("8");
    AsmFuncLabel(global_heap_cursor);
    AsmDq("0");
    AsmEmptyLine;

    AsmAlign("8");
    AsmFuncLabel(global_start_of_heap);
    AsmDq("0");
    AsmEmptyLine;

    AsmAlign("8");
    AsmFuncLabel(global_end_of_heap);  
    AsmDq("0");
    AsmEmptyLine;

    AsmAlign("8");
    AsmFuncLabel(global_start_of_stack);
    AsmDq("0");
    AsmEmptyLine;

    AsmAlign("8");
    AsmFuncLabel(global_end_of_stack);
    AsmDq("0");
    AsmEmptyLine;
    
    
  ] @ base_closures @
  [
    AsmEmptyLine;
    AsmSection(".text");
    AsmGlobal(global_bird_main);
    AsmGlobal(global_start_of_heap);
    AsmGlobal(global_end_of_heap);
    AsmGlobal(global_start_of_stack);
    AsmGlobal(global_end_of_stack);
    AsmGlobal(global_heap_cursor);
    AsmExtern(global_stopWithError);
    AsmExtern(global_printValue);
    AsmExtern(global_gc);
    
    
  ] @
  declarations @
  [
    AsmFuncLabel(global_bird_main);
    AsmPush(ArgRegister(RBP)) ;
    AsmMov( ArgRegister(RBP), ArgRegister(RSP)) ;
    AsmSub( ArgRegister(RSP), ArgConstant(string_of_int stack_mem)) ;

    (* Set global variables *)
    AsmMov( ArgMemory(AddrByLabel(global_start_of_stack)), ArgRegister(RBP)) ;

    AsmMov( ArgMemory(AddrByLabel(global_heap_cursor)), ArgRegister(RDI)) ;

    AsmMov( ArgMemory(AddrByLabel(global_start_of_heap)), ArgRegister(RDI)) ;
    (* RSI passes in the heap size *)
    AsmAdd( ArgRegister(RDI), ArgRegister(RSI)); 
    AsmMov( ArgMemory(AddrByLabel(global_end_of_heap)), ArgRegister(RDI));

    
    
    (* Clean the stack *)
    AsmXor( ArgRegister(RAX), ArgRegister(RAX) );
    AsmMov( ArgRegister(RCX), ArgConstant(string_of_int stack_mem) );
    AsmSar( ArgRegister(RCX), ArgConstant("3") );
    AsmMov( ArgRegister(RDI), ArgRegister(RSP));
    AsmRep("stosq");
    
    
  ] @
  instructions @ [
    AsmMov( ArgRegister(RSP), ArgRegister(RBP)) ;
    AsmPop( ArgRegister(RBP)) ;
  AsmRet;
  AsmEmptyLine]
;;
let compile_to_assembly_code (prog : program) : string =
  let _func_dict = check_well_formed prog in
  let converted_prog = closure_convert_program prog in 
  let marked_tail_prog = mark_tail_program converted_prog in
  let instructions = compile_program marked_tail_prog in
  let instruction_code = code_of_instruction_list instructions 0 in
  (* "section .data\n" ^
  "align 8\n" ^
  "heap_cursor:\n" ^
  "dq 0\n\n" ^

  "section .text\n" ^
  "global bird_main\n" ^
  "extern stopWithError\n" ^ 
  "bird_main:\n" ^ *)
  instruction_code 
;;
