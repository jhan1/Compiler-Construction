open Batteries;;
open HatchLanguage;;
open Environment;;

open Printf;;

open Asts;;

exception IllFormed of string list;;

(* dictionary type is used to track functions defined and their numbers of parameters *)
type dictionary = int Map.String.t;;


(* Get the size of a list *)
let rec get_lstSize (lst : 'a list) : int =
   match lst with
   | [] -> 0
   | _::rest ->
    1 + get_lstSize(rest) 
;;


let rec add_init_environment (dec_list : declaration list) : environment = 
  match dec_list with 
  | [] -> empty_environment
  | dec :: rest -> (
    match dec with 
    | DFunction(func, _, _) ->
      let env = add_init_environment rest in 
      allocate_global_variable func env
  )
;;
(* Generate the environment for a function, which only has all the params *)
let rec get_new_env_from_param_list (param_list : string list) (env : environment) : environment =
  match param_list with
  | [] -> env
  | param::rest ->
    let new_env = allocate_named_variable param env in
    get_new_env_from_param_list rest new_env
;;


(* Check compile error is expressions. *)
let rec check_exp_error (e : expr) (env : environment) (dict : dictionary) : string list =
  match e with
  | EInt(_) 
  | EBool(_) -> []
  | EUnaryOp(_, expr) -> check_exp_error expr env dict
  | EBinaryOp(_, expr_1, expr_2) -> 
    check_exp_error expr_1 env dict @ 
    check_exp_error expr_2 env dict
  | ELet(key, expr_1, expr_2) -> 
    check_exp_error expr_1 env dict @
    let new_env = allocate_named_variable key env in 
    check_exp_error expr_2 new_env dict
  | EVar(key) -> 
    (try 
      let _, env' = env in 
      let _ = Map.String.find key env' in
      []
    with
      Not_found -> ["Unbound variable "^ key ^ "."])

  | EIf(expr_1, expr_2, expr_3) -> 
    check_exp_error expr_1 env dict @ 
    check_exp_error expr_2 env dict @ 
    check_exp_error expr_3 env dict

  (* | ECall(key, expr_lst) -> 
    let rec check_exp_lst_error (expr_lst: expr list): string list = 
      match expr_lst with
      | [] -> []
      | e :: rest -> check_exp_error e env dict @ check_exp_lst_error rest in 
    check_exp_lst_error expr_lst @
    (try 
      let n_param = Map.String.find key dict in
      if n_param != get_lstSize expr_lst then 
        ["Function " ^ key ^ " is called with an incorrect number of arguments. "]
      else 
        []
    with
      Not_found -> ["Function " ^ key ^ " is not defined."])  *)
      
  | ETuple(expr_lst) -> 
    let rec check_exp_lst_error (expr_lst: expr list): string list = 
    match expr_lst with
    | [] -> []
    | e :: rest -> check_exp_error e env dict @ check_exp_lst_error rest in 
    check_exp_lst_error expr_lst
  (* TODO: Complete EAppl *)
  | EAppl(expr_1, expr_2, _) ->     
    check_exp_error expr_1 env dict @ 
    check_exp_error expr_2 env dict
  
  | ELambda(x, expr) ->
    let new_env = allocate_named_variable x env in
    check_exp_error expr new_env dict
  
  | ESet( expr_1, expr_2, expr_3 ) ->
    check_exp_error expr_1 env dict @ 
    check_exp_error expr_2 env dict @
    check_exp_error expr_3 env dict
;;

(* Check duplicate parameter in a function *)
let rec check_dup_param (func_name : string) (param_list : string list) (dict: dictionary) : string list =
  match param_list with
  | [] -> []
  | param :: rest -> 
    try 
        let _ = Map.String.find (param) (dict)  in
        ["Function " ^ func_name ^ " declares a duplicate parameter " ^ param ^ "."] @ check_dup_param func_name rest dict
      with
        Not_found ->
        let new_dict = Map.String.add (param) 1 dict in
        check_dup_param func_name rest new_dict
;;

(* Check whether there is duplicate definition of function, or duplicate parameter in a function.  *)
let rec check_func_def (dec_lst : declaration list) (dict: dictionary) : (string list * dictionary)=
  match dec_lst with
  | [] -> [], dict
  | dec :: rest -> (
    match dec with
    | DFunction (func_name, param_lst, _) -> 
      let errors, dict' = (
      try 
      let _ = Map.String.find (func_name) (dict)  in
        ["Duplicate definition of function " ^ func_name ^ "."] , dict
      with  
        Not_found -> 
          let new_dict = Map.String.add (func_name) (get_lstSize param_lst) dict in
          let errors = check_dup_param func_name param_lst Map.String.empty in
          errors, new_dict
      ) in 
      let errors_2, new_dict = check_func_def rest dict' in 
      errors @ errors_2, new_dict
  )
;;



let check_dec_error (dec : declaration) (dict : dictionary) (env : environment): string list =
  match dec with
  | DFunction (_func_name, param_list, e) -> 
    check_exp_error e (get_new_env_from_param_list param_list env) dict
;;

let rec check_dec_lst_error (dec_lst : declaration list) (dict : dictionary) (env: environment): string list =
  match dec_lst with
  | [] -> []
  | dec::rest ->
    check_dec_error dec dict env @ check_dec_lst_error rest dict env
  ;;

(* This function produces a list of compile-time errors found in a program. *)


let check_program_for_errors (p : program) : string list * dictionary =
  let empty_dict = Map.String.empty in
  match p with
  | Program (dec_lst,e) -> 
    let errors, func_dict = check_func_def dec_lst empty_dict in
    let init_environment = add_init_environment dec_lst in 
    let errors_2 = check_dec_lst_error dec_lst func_dict init_environment in 
    errors @ errors_2 @ check_exp_error e init_environment func_dict, func_dict
;;

(* This function will check a program for compile-time errors.  If any errors
   are found, an IllFormed exception is thrown.  Otherwise, unit is returned. *)
let check_well_formed (p : program) : dictionary =
  let errors, func_dict = check_program_for_errors p in
  if List.is_empty errors then func_dict else raise (IllFormed errors)
;;
