(* This file contains the data structure and functions we will use to manage
   the environment as we compile our code. *)

open Batteries;;

open Assembly;;

(* This data type represents the kind of environment we will use in our
   compiler.  This is a pair between the next free stack address and a
   dictionary mapping variable names to their offsets. *)
type environment = int * argument Map.String.t;;

(* This defines an exception which should be thrown if we try to read from an
   environment something that doesn't exist. *)
exception UnboundVariable of string * environment;;

(* The value representing an empty environment.  The map is empty and zero
   temporary locations are in use. *)
let empty_environment : environment = (-8, Map.String.empty);;

let get_func_name ( func : string ) : string =
   "fun_" ^ func
 ;;
 
 let get_closure_name ( func : string ) : string =
   "closure_of_" ^ func
 ;;
(* A function to allocate a space for a named variable from the environment.
   This function takes a variable name and an environment.  It returns the
   new environment after that allocation.
*)
let name_num_params = "$NUM_PARAMS$";;

let set_n_params ( n : int ) (env : environment): environment =
   let offset, map = env in 
      offset, (Map.String.add (name_num_params) (ArgConstant(string_of_int n)) map)

let allocate_global_variable (name : string) (env : environment) : environment =
   let offset, map = env in 
      offset, (Map.String.add (name) (ArgLabelOffset(get_closure_name name, 1) ) map)
;;
let allocate_named_variable (name : string) (env : environment) : environment =
  (* ignore name; ignore env; failwith "TODO: allocate_named_variable" TODO: delete this line *)
  let offset, map = env in
   (* match map with
   | pattern -> pattern *)
      offset-8,(Map.String.add (name) (ArgMemory(AddrByRegisterOffset(RBP, (offset)))) map)
;;

(* let allocate_named_param (name : string) (env : environment) : environment =
   let offset, map = env in
    (* match map with
    | pattern -> pattern *)
       offset+8,(Map.String.add (name) (ArgMemory(AddrByRegisterOffset(RBP, (offset)))) map)
;; *)
(* A function to find a previously-allocated space in an environment based upon
   the name of the variable for which that space was allocated.  If the variable
   was not previously allocated, then UnboundVariable is raised.
*)
let find_named_variable (name : string) (env : environment) : argument =
   (* ignore name; ignore env; failwith "TODO: find_named_variable" TODO: delete this line *)
   let _, map = env in 
   try 
      Map.String.find (name) (map)  
   with  
      Not_found -> raise (UnboundVariable (name, env) )
;;

(* A function to allocate space for a temporary variable from the environment.
   This function does not require a variable name because the temporary space is
   being allocated for the compiler and will not be associated with a name.
   Given an environment, it returns a new, temporary address as well as a new
   environment after that allocation.
*)
let allocate_temp_variable (env : environment) : argument * environment =
  let offset, map = env in 
   let addr = AddrByRegisterOffset(RBP, (offset)) in 
   let arg = (ArgMemory(addr)) in 
   (* arg, (offset-8, (Map.String.add (code_of_address addr ) arg map)) *)
   arg, (offset-8, map)
;;

(* A function to print an environment.  This is provided to you for debugging
   purposes. *)
let string_of_environment (env : environment) : string =
  let (next_free, dict) = env in
  let mappings = List.of_enum (Map.String.enum dict) in
  let rec string_of_mappings ms =
    match ms with
    | [] -> ""
    | (name, address)::ms' ->
      (Printf.sprintf
         "%s stored at %s\n"
         name
         (code_of_argument address)
      ) ^ string_of_mappings ms'
  in
  (Printf.sprintf "Next free offset: %d\n" next_free) ^
  string_of_mappings mappings
;;
