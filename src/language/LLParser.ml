(*
This file defines an LL parser for Pyrrhuloxia.
*)

open Batteries;;

open Asts;;
open LLLexer;;

(** This exception should be raised if an error occurs during parsing. *)
exception ParserError of string;;

(** A helper function which attempts to parse the prefix of a token stream into
    an expression.  It takes as an argument a list of mini-parsers: functions
    which *try* to recognize a specific expression and raise a ParserError if
    they fail. *)
let rec parse_first_of
    (parsers : (token list -> expr * token list) list)
    (input : token list)
  : expr * token list =
  match parsers with
  | [] ->
    (* We have no routines to use to find a parser.  Give up, producing an error
       message to hint to the user where we were in the file. *)
    let max_error_length = 4 in
    let first_part_of_token_stream = List.take max_error_length input in
    let stream_string =
      String.join ", " (List.map show_token first_part_of_token_stream)
    in
    raise (ParserError(
        Printf.sprintf "Failed to parse expression from tokens: %s"
          stream_string))
  | parser :: parsers' ->
    try
      (* If this parser successfully produces a result, use it! *)
      parser input
    with
    | ParserError _ ->
      (* This parser failed.  Let's try the next one. *)
      parse_first_of parsers' input
;;

(** A routine which attempts to parse a general expression. *)
let rec parse_expr (input : token list) : expr * token list =
  parse_first_of
    [ parse_let_expr;
      parse_if_expr;
      parse_or_expr;
    ]
    input

and parse_additive_expr (input : token list) : expr * token list =
    (* Read the first primary expr. *)
    let (e, input') = parse_multiplicative_expr input in
    (* In a loop, find each following "+expr" or "-expr".  Put them in a list and
       return that along with the remaining tokens. *)
    let rec loop (loop_input : token list)
      : (binary_operator * expr) list * token list =
      match loop_input with
      | TokPlus :: loop_input' ->
        (* A + is next.  See if we can get an expression after it. *)
        let (e, loop_input'') = parse_multiplicative_expr loop_input' in
        let (rest_exprs, rest_input) = loop loop_input'' in
        (* We found a + and an expression.  Store them and keep trying. *)
        ((OpPlus, e) :: rest_exprs, rest_input)
  
      | TokMinus :: loop_input' ->
        (* A - is next.  See if we can get an expression after it. *)
        let (e, loop_input'') = parse_multiplicative_expr loop_input' in
        let (rest_exprs, rest_input) = loop loop_input'' in
        (* We found a + and an expression.  Store them and keep trying. *)
        ((OpMinus, e) :: rest_exprs, rest_input)
  
      | _ ->
        (* We couldn't find a "+ expr" or "- expr", but that's okay; we'll just use what we
           have. *)
        ([], loop_input)
    in
    (* Find all of the operations we want to attach to this expr. *)
    let (op_exprs, input'') = loop input' in
    (* For each "+expr" or "-expr", build an AST from left to right which
       describes these operations. *)
    let result =
      List.fold_left
        (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
        e
        op_exprs
    in
    (result, input'')
(** A routine which attempts to parse an additive expression. *)
and parse_multiplicative_expr (input : token list) : expr * token list =
  (* Read the first primary expr. *)
  let (e, input') = parse_primary_expr input in
  (* In a loop, find each following "+expr" or "-expr".  Put them in a list and
     return that along with the remaining tokens. *)
  let rec loop (loop_input : token list)
    : (binary_operator * expr) list * token list =
    match loop_input with
    | TokTimes :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_primary_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpTimes, e) :: rest_exprs, rest_input)

    | _ ->
      (* We couldn't find a "+ expr" or "- expr", but that's okay; we'll just use what we
         have. *)
      ([], loop_input)
  in
  (* Find all of the operations we want to attach to this expr. *)
  let (op_exprs, input'') = loop input' in
  (* For each "+expr" or "-expr", build an AST from left to right which
     describes these operations. *)
  let result =
    List.fold_left
      (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
      e
      op_exprs
  in
  (result, input'')

and parse_comparison_expr (input : token list) : expr * token list =
  (* Read the first primary expr. *)
  let (e, input') = parse_additive_expr input in
  (* In a loop, find each following "+expr" or "-expr".  Put them in a list and
     return that along with the remaining tokens. *)
  let rec loop (loop_input : token list)
    : (binary_operator * expr) list * token list =
    match loop_input with
    | TokGreaterThan :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_additive_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpGreaterThan, e) :: rest_exprs, rest_input)

    | TokLessThan :: loop_input' ->
      (* A - is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_additive_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpLessThan, e) :: rest_exprs, rest_input)
    
    | TokEqualTo :: loop_input' ->
      (* A - is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_additive_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpEqualTo, e) :: rest_exprs, rest_input)

    | _ ->
      (* We couldn't find a "+ expr" or "- expr", but that's okay; we'll just use what we
         have. *)
      ([], loop_input)
  in
  (* Find all of the operations we want to attach to this expr. *)
  let (op_exprs, input'') = loop input' in
  (* For each "+expr" or "-expr", build an AST from left to right which
     describes these operations. *)
  let result =
    List.fold_left
      (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
      e
      op_exprs
  in
  (result, input'')

and parse_and_expr (input : token list) : expr * token list =
  (* Read the first primary expr. *)
  let (e, input') = parse_comparison_expr input in
  (* In a loop, find each following "+expr" or "-expr".  Put them in a list and
     return that along with the remaining tokens. *)
  let rec loop (loop_input : token list)
    : (binary_operator * expr) list * token list =
    match loop_input with
    | TokAnd :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_comparison_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpAnd, e) :: rest_exprs, rest_input)

    | _ ->
      (* We couldn't find a "+ expr" or "- expr", but that's okay; we'll just use what we
         have. *)
      ([], loop_input)
  in
  (* Find all of the operations we want to attach to this expr. *)
  let (op_exprs, input'') = loop input' in
  (* For each "+expr" or "-expr", build an AST from left to right which
     describes these operations. *)
  let result =
    List.fold_left
      (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
      e
      op_exprs
  in
  (result, input'')

and parse_or_expr (input : token list) : expr * token list =
  (* Read the first primary expr. *)
  let (e, input') = parse_and_expr input in
  (* In a loop, find each following "+expr" or "-expr".  Put them in a list and
     return that along with the remaining tokens. *)
  let rec loop (loop_input : token list)
    : (binary_operator * expr) list * token list =
    match loop_input with
    | TokOr :: loop_input' ->
      (* A + is next.  See if we can get an expression after it. *)
      let (e, loop_input'') = parse_and_expr loop_input' in
      let (rest_exprs, rest_input) = loop loop_input'' in
      (* We found a + and an expression.  Store them and keep trying. *)
      ((OpOr, e) :: rest_exprs, rest_input)

    | _ ->
      (* We couldn't find a "+ expr" or "- expr", but that's okay; we'll just use what we
         have. *)
      ([], loop_input)
  in
  (* Find all of the operations we want to attach to this expr. *)
  let (op_exprs, input'') = loop input' in
  (* For each "+expr" or "-expr", build an AST from left to right which
     describes these operations. *)
  let result =
    List.fold_left
      (fun e1 (op,e2) -> EBinaryOp(op,e1,e2))
      e
      op_exprs
  in
  (result, input'')
(** A routine which attempts to parse a primary expression. *)
and parse_primary_expr (input : token list) : expr * token list =
  parse_first_of
    [ parse_int_expr;
      parse_bool_expr;
      parse_paren_expr;
      parse_identifier_expr;
      parse_after_expr;
      parse_before_expr;
      parse_print_expr;
      parse_isint_expr;
      parse_isbool_expr;
    ]
    input

(** A routine which attempts to parse the integer production of a primary
    expression. *)
and parse_int_expr (input : token list) : expr * token list =
  match input with
  | TokInt n :: input' ->
    (EInt n, input')
  | TokMinus :: TokInt n :: input' ->
    (EInt (-n), input')
  | _ ->
    raise (ParserError("Failed to parse integer"))

and parse_bool_expr (input : token list) : expr * token list =
  match input with
  | TokTrue :: input' ->
    (EBool true, input')
  | TokFalse :: input' ->
    (EBool false, input')
  | _ ->
    raise (ParserError("Failed to parse bool"))

and parse_identifier_expr (input : token list) : expr * token list =
  match input with
  | TokIdentifier str:: input' ->
    (EVar str, input')
  | _ ->
    raise (ParserError("Failed to parse identifier"))

(** A routine which attempts to parse the parenthesis production of a primary
    expression. *)
and parse_paren_expr (input : token list) : expr * token list =
  match input with
  | TokOpenParen :: input' ->
    begin
      let (e, input'') = parse_expr input' in
      match input'' with
      | TokCloseParen :: input''' ->
        (e, input''')
      | _ ->
        raise (ParserError("Failed to parse expression: missing close paren"))
    end
  | _ ->
    raise (ParserError("Failed to parse expression: missing open paren"))

(* TODO: define your parsing helper functions here *)

and parse_after_expr (input : token list) : expr * token list =
  match input with
  | TokAfter :: input' ->
    begin
      try
        let (e, input'') = parse_paren_expr input' in 
        ( (EUnaryOp(OpAfter, e)), input'')
        (* e, input'' *)
      with
        | ParserError _ ->
          raise (ParserError("Failed to parse after"))
    end
  | _ ->
    raise (ParserError("Failed to parse after"))

and parse_before_expr (input : token list) : expr * token list =
  match input with
  | TokBefore :: input' ->
    begin
      try
        let (e, input'') = parse_paren_expr input' in 
        ( (EUnaryOp(OpBefore, e)), input'')
        (* e, input'' *)
      with
        | ParserError _ ->
          raise (ParserError("Failed to parse before"))
    end
  | _ ->
    raise (ParserError("Failed to parse before"))  

and parse_print_expr (input : token list) : expr * token list =
  match input with
  | TokPrint :: input' ->
    begin
      try
        let (e, input'') = parse_paren_expr input' in 
        ( (EUnaryOp(OpPrint, e)), input'')
        (* e, input'' *)
      with
        | ParserError _ ->
          raise (ParserError("Failed to parse print"))
    end
  | _ ->
    raise (ParserError("Failed to parse print"))  

and parse_isint_expr (input : token list) : expr * token list =
  match input with
  | TokIsInt :: input' ->
    begin
      try
        let (e, input'') = parse_paren_expr input' in 
        ( (EUnaryOp(OpIsInt, e)), input'')
        (* e, input'' *)
      with
        | ParserError _ ->
          raise (ParserError("Failed to parse isint"))
    end
  | _ ->
    raise (ParserError("Failed to parse isint"))  

and parse_isbool_expr (input : token list) : expr * token list =
  match input with
  | TokIsBool :: input' ->
    begin
      try
        let (e, input'') = parse_paren_expr input' in 
        ( (EUnaryOp(OpIsBool, e)), input'')
        (* e, input'' *)
      with
        | ParserError _ ->
          raise (ParserError("Failed to parse isbool"))
    end
  | _ ->
    raise (ParserError("Failed to parse isbool"))  

and parse_if_expr (input : token list) : expr * token list = 
    match input with
    | TokIf :: input' -> 
      begin
        try
          let condition, input'' = parse_expr input' in 
          match input'' with 
          | TokThen :: input''' -> 
            begin
            let expr1, input'''' = parse_expr input''' in 
            match input'''' with
            | TokElse :: input''''' ->
              let expr2, rest = parse_expr input''''' in 
              EIf(condition, expr1, expr2), rest              
            | _ -> raise (ParserError("Failed to parse if_then_else"))  
            end
          | _ -> raise (ParserError("Failed to parse if_then_else"))  

        with
        | ParserError _ ->
          raise (ParserError("Failed to parse if_then_else"))  
      end
    | _  ->
      raise (ParserError("Failed to parse if_then_else"))  

and parse_let_expr (input : token list) : expr * token list = 
  match input with
  | TokLet :: input' -> 
    begin
      match input' with
      | TokIdentifier (str) :: TokEqualTo :: input'' ->
        begin
          let expr1, input''' = parse_expr input'' in 
          begin
            match input''' with 
            | TokIn :: input'''' ->
              let expr2, rest = parse_expr input'''' in 
              ELet(str, expr1, expr2), rest
            | _ ->
              raise (ParserError("Failed to parse let_=_in"))
          end
        end

      | _ ->
        raise (ParserError("Failed to parse let_=_in"))  

    end
  | _  ->
    raise (ParserError("Failed to parse let_=_in"))  
;;
(** This function attempts to transform a list of tokens into a program.  If
    this process is unsuccessful, a ParserError is raised. *)
let parse (tokens : token list) : program =
  let (e, extras) = parse_expr tokens in
  if List.is_empty extras then
    Program([],e)
  else
    let stream_string = String.join ", " (List.map show_token extras) in
    raise (ParserError(Printf.sprintf "Extraneous tokens during parse: %s"
                         stream_string))
;;
