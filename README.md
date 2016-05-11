# stl - Simply Typed Lambda Calculus Interpreter

`lfo` is an interpreter for the simply typed lambda calculus. 
The implementation is Church-style (explicitly typed, no type erasure, evaluation on typing derivations).

## Features / Project Status

Feature                   | Status
------------------------- | ---------------------------
Function Types            | :white_check_mark:
Unit Type (Top)	   	  | :white_check_mark:
Empty Type (Bot)          | :white_check_mark:
Bool                      | :red_circle:
Integer                   | :red_circle:
Float                     | :red_circle:
Char                      | :red_circle:
String                    | :red_circle:
Products                  | :white_check_mark:
Tuples                    | :red_circle:
Records                   | :red_circle:
Sums                      | :white_check_mark:
Variants                  | :red_circle:
Let Bindings              | :white_check_mark:
Type Annotations          | :white_check_mark:

Properties: Strong Normalization (No general recursion)  
Because of the strong normalization property the system can be seen as a proof assistant
for propositional constructive logic.

## Grammar (of implemented features)

    exp =
          v
        | exp exp
        | λv:T.exp 
        | let v = exp in exp
        | {exp,exp}
        | fst exp
        | snd exp
        | left exp : T
        | right exp : T
        | case exp inl v ⇒ exp inr v ⇒ exp
        | exp : T
        | triv
    
    Type =
          V
        | ⊤
        | ⊥
        | Type → Type
        | Type × Type
        | Type + Type

## Usage

The interface is pretty bare bones at the moment -- `lfo` reads an expression from `stdin` 
and returns its type on `stderr` and the normalform on `stdout`.
