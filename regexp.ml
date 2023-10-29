open List
open Nfa
open Sets

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

(*creates an nfa based on the regular expression*)
let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
  match regexp with
    | Empty_String -> let s0 = fresh () in
                      let nfa = {
                        sigma = [];
                        qs = [s0];
                        q0 = s0;
                        fs = [s0];
                        delta = []} in nfa
    | Char c -> let s0 = fresh () in
                let s1 = fresh () in
                let nfa = {
                  sigma = [c]; (*creates epsilon transition*)
                  qs = [s0; s1]; (*creates 2 new states*)
                  q0 = s0; (*creates starting state*)
                  fs = [s1];
                  delta = [(s0, Some c, s1)]} in nfa
    | Union (a, b) -> let nfa1 = regexp_to_nfa a in
                      let nfa2 = regexp_to_nfa b in
                      let s0 = fresh () in
                      let s1 = fresh () in
                      let new_nfa = {
                        sigma = union nfa1.sigma nfa2.sigma; (*merges the alphabets of the 2 nfas*)
                        qs = union (union nfa1.qs nfa2.qs) [s0; s1]; (*merges the states of the 2 nfas, adds 2 new states*)
                        q0 = s0; (*new initial state*)
                        fs = [s1]; (*new final state*)
                        (*merges the transitions of the 2 nfas, adds new transitions*)
                        delta = union (union nfa1.delta nfa2.delta) [(s0, None, nfa1.q0); 
                        (s0, None, nfa2.q0); (List.hd nfa1.fs (*first element of the list*), None, s1); 
                        (List.hd nfa2.fs, None, s1)]
                      } in new_nfa
    | Concat (a, b) -> let nfa1 = regexp_to_nfa a in
                       let nfa2 = regexp_to_nfa b in
                       let new_nfa = {
                         sigma = union nfa1.sigma nfa2.sigma;
                         qs = union nfa1.qs nfa2.qs;
                         q0 = nfa1.q0;
                         fs = nfa2.fs;
                         (*creates an epsilon between the 2 final states*)
                         delta = union (union nfa1.delta nfa2.delta) [(List.hd nfa1.fs, None, nfa2.q0)]
                       } in new_nfa
    | Star a -> let nfa1 = regexp_to_nfa a in
                let s0 = fresh () in
                let s1 = fresh () in
                let new_nfa = {
                  sigma = nfa1.sigma;
                  qs = union nfa1.qs [s0; s1]; (*adds new states*)
                  q0 = s0; (*new initial state*)
                  fs = [s1]; (*new final state*)
                  (*new transitions*)
                  delta = union nfa1.delta [(List.hd nfa1.fs, None, s1);
                  (s0, None, nfa1.q0); (s0, None, s1); (s1, None, s0)]
                } in new_nfa

(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")

(*takes a string for a regular expression, parses the string, and outputs its equivalent regexp*)
let string_to_regexp str = parse_regexp @@ tokenize str

(*takes a string for a regular expression, parses the string, 
  converts it into a regexp, and transforms it to an nfa*)
(*regexp_to_nfa function must be working*)
let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
