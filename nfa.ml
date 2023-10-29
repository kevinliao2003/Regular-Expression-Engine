open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

let rec map f xs = match xs with
| [] -> []
| x :: xt -> (f x)::(map f xt)

let rec fold f a xs = match xs with
| [] -> a
| x :: xt -> fold f (f a x) xt

let rec fold_right f xs a = match xs with
| [] -> a
| x :: xt -> f x (fold_right f xt a)

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec move_helper nfa qs s new_lst =
  (*loops through the qs list*)
  match qs with
  | [] -> new_lst
  | h::t -> let list = fold (fun acc x -> match x with
    | (a, Some b, c) -> if a = h && Some b = s then insert c acc else acc
    | (d, None, e) -> if d = h && s = None then insert e acc else acc) [] nfa.delta in
    move_helper nfa t s (union (new_lst@list) (list))

(*move nfa_ex [0] (Some 'a') = [1] (* nfa_ex is the NFA defined above *)
move nfa_ex [1] (Some 'a') = []
move nfa_ex [2] (Some 'a') = []
move nfa_ex [0;1] (Some 'a')  = [1]
move nfa_ex [1] None = [2]*)
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  move_helper nfa qs s []

let rec e_closure_helper nfa qs new_lst =
  match qs with
  | [] -> new_lst
  | h::t -> let epsilons = (move nfa [h] None) in
    e_closure_helper nfa (union t (diff (epsilons) (new_lst))) (union (epsilons) (new_lst))

(*e_closure nfa_ex [0] = [0]
e_closure nfa_ex [1] = [1;2]
e_closure nfa_ex [2]  = [2]
e_closure nfa_ex [0;1] = [0;1;2]*)
let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  e_closure_helper nfa qs qs

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(*new_states nfa_ex [0] = [[1; 2]]
new_states dfa_ex [0; 1] = [[1]; [0]; [2]]*)
let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  fold (fun acc x -> let states = (e_closure nfa (move nfa qs (Some x))) in states::acc) [] nfa.sigma

(*new_trans dfa_ex [0; 1] = [([0; 1], Some 'a', [1]); ([0; 1], Some 'b', [0]); ([0; 1], Some 'c', [2])]*)
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  fold (fun acc x -> let epsilons = e_closure nfa (move nfa qs (Some x)) in (qs, Some x, epsilons)::acc) [] nfa.sigma

(*new_finals dfa_ex [0; 1; 2] = [[0; 1; 2]]
new_finals dfa_ex [0; 1] = []*)
let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if intersection (nfa.fs) (qs) = [] then [] else qs::[]

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  | [[]] -> dfa
  | [] -> dfa
  | h::t -> let states = (new_states nfa h) in (*finds the new states*)
            let diff = diff states dfa.qs in (*ignores the states that've already been processed*)
            let transitions = new_trans nfa h in (*finds the new transitions*)
            let final_states = new_finals nfa h in (*determines if the state is final*)
            nfa_to_dfa_step nfa {
              sigma = dfa.sigma;
              qs = union dfa.qs diff; (*updates the dfa states*)
              q0 = dfa.q0;
              fs = union dfa.fs final_states;
              delta = union dfa.delta transitions} (union t diff) (*updates the states to be visited*)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = 
  let initial_state = e_closure nfa [nfa.q0] in
  let dfa = {
    sigma = nfa.sigma; 
    qs = [initial_state];
    q0 = initial_state;
    fs = [];
    delta = []} in
  nfa_to_dfa_step nfa dfa [initial_state]

(*chars - split string into characters, state - current state to examine*)
let rec accept_helper nfa chars state =
  match chars with
  | [] -> let epsilons = e_closure nfa state in (*checks for epsilons that lead to the final state*)
          if new_finals nfa epsilons = [] then false (*checks if the last state is final*)
          else true
  | h::t -> let epsilons = e_closure nfa state in
            let move = move nfa epsilons (Some h) in
            let move_epsilons = e_closure nfa move in
            if move_epsilons = [] then false else accept_helper nfa t move_epsilons

(*(aE)*|a|b*)
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
    let chars = explode s in (*splits the string into characters*)
    accept_helper nfa chars [nfa.q0]