(*
This file defines a lexer for an LL parser generator for Pyrrhuloxia.
*)

open Batteries;;

(** An exception type which should be raised when lexing fails. *)
exception LexerError of string;;

(** The type of tokens produced by this lexer. *)
type token =
  | TokInt of int
  | TokPlus
  | TokMinus
  | TokOpenParen
  | TokCloseParen
  (* TODO: add more token types here *)
  | TokTrue
  | TokFalse
  | TokAfter
  | TokBefore
  | TokPrint
  | TokIsBool
  | TokIsInt
  | TokTimes
  | TokLessThan
  | TokGreaterThan
  | TokEqualTo
  | TokAnd
  | TokOr 
  | TokIdentifier of string
  | TokLet 
  | TokIn 
  | TokIf 
  | TokThen 
  | TokElse
[@@deriving eq, ord, show];;

(** A helper function which attempts to lex the prefix of a string into a token.
    It takes as an argument a list of mini-lexers: functions which *try* to
    recognize a specific token and raise a LexerError if they fail. *)
let rec tokenize_first_of
    (tokenizers : (char list -> token * char list) list)
    (input : char list)
  : token * char list =
  match tokenizers with
  | [] ->
    (* We have no routines to use to find a token.  Give up, producing an error
       message to hint to the user where we were in the file. *)
    let max_error_length = 20 in
    let first_part_of_string =
      String.of_list (List.take max_error_length input)
    in
    raise (LexerError(Printf.sprintf "Unrecognized token starting at: \"%s\""
                        first_part_of_string))
  | tokenizer :: tokenizers' ->
    try
      (* If this tokenizer successfully produces a result, use it! *)
      tokenizer input
    with
    | LexerError _ ->
      (* This tokenizer failed.  Let's try the next one. *)
      tokenize_first_of tokenizers' input
;;

(** This routine discards whitespace from the input stream. *)
let discard_whitespace (input : char list) : char list =
  List.drop_while Char.is_whitespace input
;;

(** This routine attempts to lex a single numeric token from the input.
    Note that this routine does NOT handle negative numbers. *)
let tokenize_int (input : char list) : token * char list =
  (* Get all of the digits from the front of the input that we can. *)
  let digits = List.take_while Char.is_digit input in
  let rest = List.drop_while Char.is_digit input in
  (* If we didn't get any digits, then the next token isn't an integer. *)
  if List.is_empty digits then
    raise (LexerError "Could not tokenize integer")
  else
    (* Convert the list of digits into a string. *)
    let digit_string = String.of_list digits in
    (* Turn that into a number. *)
    let number = int_of_string digit_string in
    (* Return a token with that number along with the rest of the input. *)
    (TokInt number, rest)
;;

(** This routine attempts to lex a single plus token from the input. *)
let tokenize_plus (input : char list) : token * char list =
  match input with
  | '+'::rest -> (TokPlus, rest)
  | _ -> raise (LexerError "Could not tokenize plus")
;;

let tokenize_minus (input : char list) : token * char list =
  match input with
  | '-'::rest -> (TokMinus, rest)
  | _ -> raise (LexerError "Could not tokenize minus")
;;

let tokenize_times (input : char list) : token * char list =
  match input with
  | '*'::rest -> (TokTimes, rest)
  | _ -> raise (LexerError "Could not tokenize times")
;;

let tokenize_lessThan (input : char list) : token * char list =
  match input with
  | '<'::rest -> (TokLessThan, rest)
  | _ -> raise (LexerError "Could not tokenize lessThan")
;;

let tokenize_GreaterThan (input : char list) : token * char list =
  match input with
  | '>'::rest -> (TokGreaterThan, rest)
  | _ -> raise (LexerError "Could not tokenize greaterThan")
;;

let tokenize_EqualTo (input : char list) : token * char list =
  match input with
  | '='::rest -> (TokEqualTo, rest)
  | _ -> raise (LexerError "Could not tokenize equalTo")
;;

let tokenize_And (input : char list) : token * char list =
  match input with
  | '&'::'&'::rest -> (TokAnd, rest)
  | _ -> raise (LexerError "Could not tokenize and")
;;

let tokenize_Or (input : char list) : token * char list =
  match input with
  | '|'::'|'::rest -> (TokOr, rest)
  | _ -> raise (LexerError "Could not tokenize or")
;;

(** This routine attemps to lex a single open parenthesis from the input. *)
let tokenize_open_paren (input : char list) : token * char list =
  match input with
  | '('::rest -> (TokOpenParen, rest)
  | _ -> raise (LexerError "Could not tokenize open parenthesis")
;;

(** This routine attemps to lex a single close parenthesis from the input. *)
let tokenize_close_paren (input : char list) : token * char list =
  match input with
  | ')'::rest -> (TokCloseParen, rest)
  | _ -> raise (LexerError "Could not tokenize close parenthesis")
;;

(* TODO: write more lexer routines here *)
(* Lexers for keywords *)
let isBody (c : char) : bool = 
  Char.is_letter c || c = '_' || Char.is_digit c
;;
let tokenize_true (input : char list) : token * char list =
  match input with
  | 't'::'r'::'u'::'e'::next::rest 
    when not (isBody next) -> (TokTrue, next::rest)
  | _ -> raise (LexerError "Could not tokenize true")
;;

let tokenize_false (input : char list) : token * char list =
  match input with
  | 'f'::'a'::'l'::'s'::'e'::next::rest 
  when not (isBody next) -> (TokFalse, next::rest)
  | _ -> raise (LexerError "Could not tokenize false")
;;

let tokenize_after (input : char list) : token * char list =
  match input with
  | 'a'::'f'::'t'::'e'::'r'::next::rest 
  when not (isBody next) ->  (TokAfter, next::rest)
  | _ -> raise (LexerError "Could not tokenize after")
;;

let tokenize_before (input : char list) : token * char list =
  match input with
  | 'b'::'e'::'f'::'o'::'r'::'e'::next::rest 
  when not (isBody next) -> (TokBefore, next::rest)
  | _ -> raise (LexerError "Could not tokenize before")
;;

let tokenize_print (input : char list) : token * char list =
  match input with
  | 'p'::'r'::'i'::'n'::'t'::next::rest 
  when not (isBody next) -> (TokPrint, next::rest)
  | _ -> raise (LexerError "Could not tokenize print")
;;

let tokenize_isInt (input : char list) : token * char list =
  match input with
  | 'i'::'s'::'i'::'n'::'t'::next::rest 
  when not (isBody next) -> (TokIsInt, next::rest)
  | _ -> raise (LexerError "Could not tokenize isInt")
;;

let tokenize_isbool (input : char list) : token * char list =
  match input with
  | 'i'::'s'::'b'::'o'::'o'::'l'::next::rest 
  when not (isBody next) -> (TokIsBool, next::rest)
  | _ -> raise (LexerError "Could not tokenize isBool")
;;

let tokenize_let (input : char list) : token * char list =
  match input with
  | 'l'::'e'::'t'::next::rest 
  when not (isBody next) -> (TokLet, next::rest)
  | _ -> raise (LexerError "Could not tokenize let")
;;

let tokenize_in (input : char list) : token * char list =
  match input with
  | 'i'::'n'::next::rest 
  when not (isBody next) -> (TokIn, next::rest)
  | _ -> raise (LexerError "Could not tokenize in")
;;

let tokenize_if (input : char list) : token * char list =
  match input with
  | 'i'::'f'::next::rest 
  when not (isBody next) -> (TokIf, next::rest)
  | _ -> raise (LexerError "Could not tokenize if")
;;

let tokenize_then (input : char list) : token * char list =
  match input with
  | 't'::'h'::'e'::'n'::next::rest 
  when not (isBody next) -> (TokThen, next::rest)
  | _ -> raise (LexerError "Could not tokenize then")
;;

let tokenize_else (input : char list) : token * char list =
  match input with
  | 'e'::'l'::'s'::'e'::next::rest 
  when not (isBody next) -> (TokElse, next::rest)
  | _ -> raise (LexerError "Could not tokenize else")
;;

(* Lexer for identifier *)
let tokenize_identifier (input : char list) : token * char list =
  (* Get all of the digits from the front of the input that we can. *)
  let isStart (c : char) : bool = 
    Char.is_letter c || c = '_'
  in
  let start = List.take_while isStart input in
  let rest = List.drop_while isStart input in
  (* If we didn't get any digits, then the next token isn't an integer. *)
  if List.is_empty start then
    raise (LexerError "Could not tokenize identifier")
  else
    (* Convert the list of digits into a string. *)
    (* let digit_string = String.of_list digits in
    (* Turn that into a number. *)
    let number = int_of_string digit_string in
    (* Return a token with that number along with the rest of the input. *)
    (TokInt number, rest) *)
    let body = List.take_while isBody rest in
    let rest' = List.drop_while isBody rest in
    let identifier = start @ body in 
    let identifier_string = String.of_list identifier in
    (TokIdentifier identifier_string, rest')
;;


(** This routine attempts to take a single token from the input stream.  If an
    unrecoverable error occurs, a LexerError is raised. *)
let tokenize (input : char list) : token * char list =
  (* TODO: modify this function as you write more lexer routines. *)
  tokenize_first_of
    [ tokenize_int;
      tokenize_plus;
      tokenize_open_paren;
      tokenize_close_paren;

      tokenize_And;
      tokenize_EqualTo;
      tokenize_GreaterThan;
      tokenize_Or;
      tokenize_after;
      tokenize_before;
      tokenize_else;
      tokenize_false;
      tokenize_if;
      tokenize_int;


      tokenize_in;
      tokenize_isInt;
      tokenize_isbool;
      tokenize_lessThan;
      tokenize_let;
      tokenize_minus;
      tokenize_print;
      tokenize_then;
      tokenize_times;
      tokenize_true;



      tokenize_identifier;
    ]
    input
;;

(** A function to lex a string.  If lexing is successful, a list of tokens is
    returned.  Otherwise, a LexerError is raised. *)
let lex (text : string) : token list =
  let raw_input = String.to_list text in
  let input = raw_input @ [' '; ' '; ' '] in 
  (*
    This function recursively takes a token from the input stream.  It builds up
    a list in *reverse* order because this allows it to tail recurse; the
    list is reversed once the routine is complete.
  *)
  let rec take_tokens (tokens_so_far : token list) (input : char list)
    : token list =
    (* Get rid of any leading whitespace. *)
    let input' = discard_whitespace input in
    (* If we're out of input, then return the list of tokens we have. *)
    if List.is_empty input' then
      tokens_so_far
    else
      (* Take a token from the input. *)
      let (token, input'') = tokenize input' in
      (* Recurse to take more tokens.  Note that the new token is put on the
         front of the list, resulting in the list being constructed in backwards
         order.  This is reversed later.  Doing things this way allows us to
         tail recurse here!  :-D *)
      take_tokens (token :: tokens_so_far) input''
  in
  List.rev (take_tokens [] input)
;;
