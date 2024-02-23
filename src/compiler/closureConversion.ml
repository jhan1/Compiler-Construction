open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;
open Wellformedness;;

let empty_set = Set.String.empty;;
let rec free_vars (e : expr ) : Set.String.t = 
  match e with
  | EInt _ -> empty_set
  | EBool _ -> empty_set
  | EUnaryOp (_, e1) -> free_vars e1
  | EBinaryOp (_, e2, e3) -> Set.String.union (free_vars e2)  (free_vars e3)
  | ETuple e_lst -> 
    let rec check_exp_lst (expr_lst: expr list): Set.String.t = 
      match expr_lst with
      | [] -> empty_set
      | e :: rest -> Set.String.union (free_vars e) (check_exp_lst rest) in 
    check_exp_lst e_lst

  | ELet (x, e1, e2) -> 
    let free_vars_in_e2 = free_vars e2 in 
    let removed_x = Set.String.remove x free_vars_in_e2 in 
    Set.String.union (free_vars e1) removed_x
      
  | EVar str -> 
    (* Set.String.add str empty_set *)
    Set.String.singleton str

  | EIf (e1, e2, e3) -> 
      Set.String.union
      (Set.String.union (free_vars e1)
      (free_vars e2) )
      (free_vars e3)

  | EAppl (e1, e2, _) -> 
      Set.String.union (free_vars e1)  (free_vars e2)

  | ELambda (x, e1) -> 
      let free_vars_in_e1 =  free_vars e1 in 
      Set.String.remove x free_vars_in_e1
  
  | ESet (e1, e2, e3) -> 
    Set.String.union
    (Set.String.union (free_vars e1)
    (free_vars e2) )
    (free_vars e3)

;;

let rec closure_convert_expression (e : expr) : expr * declaration list = 
  match e with
  | EInt _ -> e, []
  | EBool _ -> e, []
  | EUnaryOp (op, e1) -> 
    let new_e, dec_lst = closure_convert_expression e1 in 
    EUnaryOp (op, new_e), dec_lst

  | EBinaryOp (op, e1, e2) -> 
    let new_e1, dec_lst_1 = closure_convert_expression e1 in
    let new_e2, dec_lst_2 = closure_convert_expression e2 in
    EBinaryOp (op, new_e1, new_e2), dec_lst_1 @ dec_lst_2

  | ETuple e_lst -> 
    let rec check_exp_lst (expr_lst: expr list): expr list * declaration list = 
      match expr_lst with
      | [] -> [], []
      | e :: rest -> 
        let new_e, dec_lst = closure_convert_expression e in
        let rest_e_lst, rest_dec_lst = check_exp_lst rest in 
        new_e :: rest_e_lst, dec_lst @ rest_dec_lst in
    let new_expr_lst, dec_lst = check_exp_lst e_lst in 
    ETuple(new_expr_lst), dec_lst

  | ELet (x, e1, e2) -> 
    let new_e1, dec_lst_1 = closure_convert_expression e1 in
    let new_e2, dec_lst_2 = closure_convert_expression e2 in
    ELet (x, new_e1, new_e2), dec_lst_1 @ dec_lst_2

  | EVar _ -> e, []

  | ESet (e1, e2, e3) -> 
    let new_e1, dec_lst_1 = closure_convert_expression e1 in
    let new_e2, dec_lst_2 = closure_convert_expression e2 in
    let new_e3, dec_lst_3 = closure_convert_expression e3 in
    ESet (new_e1, new_e2, new_e3), dec_lst_1 @ dec_lst_2 @ dec_lst_3

  | EIf (e1, e2, e3) -> 
    let new_e1, dec_lst_1 = closure_convert_expression e1 in
    let new_e2, dec_lst_2 = closure_convert_expression e2 in
    let new_e3, dec_lst_3 = closure_convert_expression e3 in
    EIf (new_e1, new_e2, new_e3), dec_lst_1 @ dec_lst_2 @ dec_lst_3

  | EAppl (e1, e2, _) -> 
    let new_e1, dec_lst_1 = closure_convert_expression e1 in
    let new_e2, dec_lst_2 = closure_convert_expression e2 in
    EAppl (new_e1, new_e2, false), dec_lst_1 @ dec_lst_2    




  | ELambda (x, e1) -> 
    (* let new_e1, dec_lst_1 = closure_convert_expression e1 in *)
    let free_var_set = free_vars e1 in 
    let free_var_without_x = Set.String.remove x free_var_set in
    let free_var_lst = Set.String.elements free_var_without_x in 
    (* let n_free_var = 1 + get_lstSize free_var_lst in  *)
    let lambda : string = fresh_name "$Anonymous" in
    let rec create_appl (free_var_lst: string list) label : expr = 
      (match free_var_lst with 
      | [] -> 
        EVar( label )
      | var :: rest ->
        let e = create_appl rest label in 
        EAppl( e, EVar(var) , false)
      )
    in 
    let new_e1, dec_lst_1 = closure_convert_expression e1 in
    let new_appl = create_appl free_var_lst lambda in 
    let var_lst_with_x = free_var_lst @ [x] in 
    new_appl, DFunction(lambda, var_lst_with_x, new_e1) :: dec_lst_1

  
;;

let closure_convert_declaration (dec : declaration) : declaration * declaration list = 
  match dec with 
  | DFunction (func_name, param_lst, e) -> 
    let new_e, dec_lst = closure_convert_expression e in 
    DFunction( func_name, param_lst, new_e), dec_lst
;;

let rec closure_convert_declaration_list (dec_lst : declaration list) : declaration list = 
  match dec_lst with 
  | [] -> []
  | dec :: rest ->
    let dec_rest = closure_convert_declaration_list rest in 
    let new_dec, new_dec_lst = closure_convert_declaration dec in 
    new_dec :: new_dec_lst @ dec_rest
;;

let closure_convert_program (prog : program) : program =
  match (prog : program) with 
  | Program (dec_lst, e) -> 
    let new_dec_lst = closure_convert_declaration_list dec_lst in 
    let new_expr, new_dec_lst' = closure_convert_expression e in 
    Program(new_dec_lst @ new_dec_lst', new_expr ) 
;;
