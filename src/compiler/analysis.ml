open Batteries;;
open HatchLanguage;;

open Assembly;;
open Asts;;
open Environment;;
open Printf;;
open Freshening;;
(* 
  Given an AST, identify caees in which constants are used as conditons. As an example: 
  if true then 4 else 6
*)

let rec find_constant_conditional (e : expr) : string list = 
  match e with
  | EInt _ 
  | EBool _ 
  | EVar _ -> []
  | EUnaryOp (_, e') -> find_constant_conditional e'
  | EBinaryOp (_, e1, e2) 
  | ELet (_, e1, e2) -> 
    find_constant_conditional e1 @ find_constant_conditional e2

  | EIf (EBool _, e2, e3) -> 
    ["Warning: constant conditional found in " ^ show_expr e] @
    find_constant_conditional e2 @
    find_constant_conditional e3 
  | EIf (e1, e2, e3) -> 
    find_constant_conditional e1 @
    find_constant_conditional e2 @
    find_constant_conditional e3 
;;
(* 
let analyze (_p : program) : string list =
  [
  ]
;; *)

(* Gievn An AST, identify cases of variable shadowing. Variable shadowing is declared using a name already in scope. For example:
   let x = 4 in 
   let x = 3 in 
   x + x
*) 
(* 
let detect_variable_shadowing (bound_vars : string list)(e : expr) : string list = 
  match e with
| EInt _ -> _
| EBool _ -> _
| EUnaryOp (_, _) -> _
| EBinaryOp (_, _, _) -> _
| ELet (x, e1, e2) -> 
  if List.mem x bound_vars then
    ["Warning: a shadowed variable..."]
  else
    ["This variable hasn't been bound yet. "]
| EVar _ -> _
| EIf (_, _, _) -> _

;; *)
