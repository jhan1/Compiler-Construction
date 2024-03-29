(** Contains the AST definitions for the Falcon language. *)

(** The unary operators in the language. *)
type unary_operator =
  | OpAfter
  | OpBefore
  | OpPrint
  | OpIsInt
  | OpIsBool
  | OpIsTuple
[@@deriving eq, ord, show]
;;

(** The binary oeprators in the language. *)
type binary_operator =
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDiv
  | OpMod
  | OpLessThan
  | OpGreaterThan
  | OpEqualTo
  | OpAnd
  | OpOr
  | OpTupleIndex
[@@deriving eq, ord, show]
;;

(** All forms of expression in the language. *)
type expr =
  | EInt of int
  | EBool of bool
  | EUnaryOp of unary_operator * expr
  | EBinaryOp of binary_operator * expr * expr
  | ETuple of expr list
  | ELet of string * expr * expr
  | EVar of string
  | EIf of expr * expr * expr
  | EAppl of expr * expr * bool (* boolean indicates tail callability *)
  | ESet of expr * expr * expr
  | ELambda of string * expr
[@@deriving eq, ord, show]
;;

(** Forms of declaration in the language. *)
type declaration =
  | DFunction of string * string list * expr
[@@deriving eq, ord, show]
;;

(** The form of programs in the language. *)
type program =
  | Program of declaration list * expr
[@@deriving eq, ord, show]
;;
