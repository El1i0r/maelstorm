open Leva_01
open Evaluator

(* Example usage *)
let () =
  (* The identity function: λx.x *)
  let identity = Abs ("x", Var "x") in

  (* Another simple function: λy.y *)
  let another_fn = Abs ("y", Var "y") in

  (* The expression to evaluate: (λx.x) (λy.y) *)
  let expression = App (identity, another_fn) in

  (* Evaluate the expression *)
  let result = eval initial_env expression in

  (* Print the results *)
  print_endline ("Evaluating expression: " ^ string_of_expr expression);
  print_endline ("Result: " ^ string_of_value result)