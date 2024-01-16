# NFA to DFA Conversion in OCaml

This OCaml code implements non-deterministic finite automata (NFA) and provides a mechanism for converting NFAs to deterministic finite automata (DFA). The code includes various utility functions and the necessary steps for the conversion process.

## File Structure

- `nfa_to_dfa.ml`: Contains the OCaml code for NFA implementation, utility functions, and the NFA to DFA conversion process.

## Types and Functions

### Types

- `('q, 's) transition`: Represents a transition in the automaton, consisting of a source state, an optional symbol, and a destination state.
- `('q, 's) nfa_t`: Represents the NFA structure with states, alphabet, initial state, final states, and transitions.

### Utility Functions

- `map f xs`: Applies a function `f` to each element in the list `xs`.
- `fold f a xs`: Applies a binary function `f` to the elements of the list `xs` starting from an accumulator `a`.
- `explode s`: Converts a string `s` to a list of characters.

### NFA Operations

- `move nfa qs s`: Computes the set of states reachable from a given set of states `qs` with an optional symbol `s`.
- `e_closure nfa qs`: Computes the epsilon closure of a set of states `qs`.

### NFA to DFA Conversion

- `new_states nfa qs`: Computes the new states in the DFA from a set of NFA states `qs`.
- `new_trans nfa qs`: Computes the new transitions in the DFA from a set of NFA states `qs`.
- `new_finals nfa qs`: Computes the new final states in the DFA from a set of NFA states `qs`.
- `nfa_to_dfa_step nfa dfa work`: Performs a step in the NFA to DFA conversion process.
- `nfa_to_dfa nfa`: Converts an NFA to a DFA.

### Acceptance Check

- `accept nfa s`: Checks whether a given string `s` is accepted by the NFA.

## Example Usage

```ocaml
(* Example NFA definition *)
let nfa_ex = {
  sigma = ['a'; 'b'];
  qs = [0; 1; 2];
  q0 = 0;
  fs = [2];
  delta = [(0, Some 'a', 1); (1, None, 2); (0, Some 'b', 2); (2, Some 'a', 1)];
}

(* Convert NFA to DFA *)
let dfa_ex = nfa_to_dfa nfa_ex

(* Check acceptance of a string *)
let result = accept dfa_ex "abba"
```

## License
```css
Feel free to customize it according to your specific needs or provide additional information about the code.
```