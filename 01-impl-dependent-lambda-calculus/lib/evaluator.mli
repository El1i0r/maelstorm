(* The type for expressions in the lambda calculus. *)
type expr =
  | Var of string
  | Abs of string * expr
  | App of expr * expr

(* The abstract type for values, which are the result of evaluation. *)
type value

(* The initial empty environment. *)
val initial_env : (string * value) list

(* Evaluates an expression in a given environment. *)
val eval : (string * value) list -> expr -> value

(* Pretty-printers for expressions and values. *)
val string_of_expr : expr -> string
val string_of_value : value -> string