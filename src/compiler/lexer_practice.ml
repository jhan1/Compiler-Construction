(* 
  A lexer for a small language
  
  <expr> :: = <integer>
          | after(<expr>)
          | <expr> + <expr>
*)

open Batteries;; 

type token = 
  | TokPlus 
  | TokInt of int 
  | TokOpenParen
  | TokCloseParen
  | TokAfter
;;

exception LexerError
let is_identifier_character (c : char) : bool =
  Char.is_digit c || Char.is_latin1 c || c == '_' || c == '/'
;;

let lex_symbol 
  (symbol_char : char)
  (token : token)
  (chars : char list)
  : token * char list =
  match chars with
  | ch::rest when ch = symbol_char -> (token, rest)
  | _ -> raise LexerError


(* let lex_plus (chars : char list) : token * char list = 
  match chars with 
  | '+'::rest -> (TokPlus, rest)
  | _ -> raise LexerError
;; *)

let lex_plus = lex_symbol '+' TokPlus;;
let lex_open_paren = lex_symbol '(' TokOpenParen;;
let lex_close_paren = lex_symbol ')' TokCloseParen;;


let rec lex_tokens (characters : char list) : token list = 
  (* match characters with
  | [] -> []
  | '+':: rest -> TokPlus :: lex_tokens rest 
  | '(':: rest -> TokOpenParen :: lex_tokens rest 
  | ')':: rest -> TokCloseParen :: lex_tokens rest 
  | 'a'::'f'::'t'::'e'::'r'::next::rest 
    when not (is_identifier_character next)->
    TokAfter :: lex_tokens (next::rest)
  | ch :: next 
      when Char.is_digit ch ->

  | _ -> raise LexerError *)
  let rec loop 
    (chars : char list) 
    (minitokenizers : (char list -> token * char list) list) 
    : token list 
    = 
    (match minitokenizers with
    | [] -> raise LexerError
    | minitokenizer::rest ->
      try
        let (token, remaining) = minitokenizer chars in 
        token :: (lex_tokens remaining)
      with 
      | LexerError ->
          loop chars rest)
  in
    loop characters [lex_plus] 
;;

let lex (text : string) : token list =
  lex_tokens (String.to_list text)
;;