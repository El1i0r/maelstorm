(* leva_01_dependently_typed *)

(* Evaluating Untyped lambda-Calculus:

  Let’s start with an evaluator for the untyped lambda-calculus. Writing an evaluator
  requires the following steps:

    - Identify the values that are to be the result of evaluation.
    - Figure out which expressions become values immediately, and which require
      computation.
    - Implement structs for the values, and use procedures for computation

  In this case, for the untyped lambda-calculus, the only values available are
  closures, and computation occurs when a closure is applied to another value. 
*)

(* Define the type for an expression in the lambda calculus *)
type expr =
  (* Expression can be a variable, like x or y *)
  | Var of string
  (*
    Expression can be a lambda abstraction, a function basically. It takes two
    arguments, a string and an expr for the function body.
  *)
  | Abs of string * expr
  (*
    Expression can be functional application, it takes two arguments both of
    type expr. So basically the function to be applied, the first expression,
    and the arguments of the function.
  *)
  | App of expr * expr

(* Values and Runtime environments:

  A closure packages an expression that has not yet been evaluated with
  the runtime environment in which the expression was created.
  A closure is a function that remembers the environment where it was
  created. The inner function can access variables from the outer
  function, it captures those variables so they don't disappear.
*)

(*
  This defines a type value which represents the result of an evaluation.
  In this simple untyped lambda calculus, the only kind of value is a
  closure, so the value type has only one constructor, Clos, which holds
  a value of type clos.
*)
type value = Clos of clos
(*
  Since value depends on clos (because it holds a clos), and clos depends
  on env and expr, all these type definitions must be tied together. This
  line defines env as a list of pairs, where each pair consists of a string
  (a variable name) and a value (the value associated with that variable).
  This is the canonical representation of a runtime environment in functional
  programming.
*)
and env = (string * value) list
(* this line defines clos as a record type. *)
and clos = {
  (*
    env: holds a value of the type env that we just defined. This stores
    the environment in which the closure was created, which is crucial for
    handling variable binding.
    var: name of the variable bound by the lambda abstraction.
    body: the code that will be evaluated when the closure is evaluated.
  *)
  env : env;
  var : string;
  body : expr;
}

(*
  Performs the operation of adding a new variable-value binding to the
  existing runtime environment. It implements the key mechanism for
  handling variable scoping and shadowing.
*)
let extend (rho : env) (x : string) (v : value) : env =
  (x , v) :: rho

(* Helper function for looking up a variable in the environment *)
let lookup (rho : env) (x : string) : value option =
  List.assoc_opt x rho

(* The Evaluator:

  The evaluator consists of two procedures: "eval" which evaluates an
  expression in a runtime environment that provides values for its free
  variables, and "apply" is responsible for applying the value of a
  function to the value of its argument.
  
  The "eval" procedure here is responsible for identifying expression.
  For abstraction, it is a value so it creates a closure and stops.
  For application, it must recursively evaluate both the function, and
  argument before handing them to "apply".
  
  The procedure "apply" performs beta reduction. What is beta reduction?
  β-reduction is basically defines how a function is applied to arguments.
  In essence it is the act of substitution of the argument value into the
  body of the function.
  
  Given an application of a function to an argument, (λx.M)N, the
  β-reduction rule states that the result is M[x:=N], which means that the
  argument expression (N) is substituted for every free occurrence of the
  parameter (x) in the function body (M).

  The way "apply" works is that:
    - Retrieve the capture environment from the closure stored within the
      "clos" record. So basically, When the lambda abstraction (λx.b) was
      first evaluated, it immediately became a closure. This closure bundled
      the function's body (b), its parameter (x), and the entire environment
      that existed at the moment of its definition (capture environment).
      
    - Extend the environment for substitution. The procedure calls "extend"
      to create a new environment. It is formed by taking the captured
      environment and adding a single new binding, that is the formal
      parameter (x) is bound to the argument (varg). This is denoted formally
      as ρc​[x↦varg​]. This is the functional equivalent of the substitution
      b[x:=varg]. Any reference to the formal parameter x during the evaluation
      of b will now immediately find varg at the front of ρnew (due to shadowing).
      Any reference to a free variable will simply fall through this new binding
      and find its original value in ρc.
      
    - And now you just evaluate the body of the function using the environment
      ρnew. The result of this final eval call is the final value of the entire
      function application. This is the value that is formally returned by the
      β-reduction. 
*)
let rec eval (rho : env) (e : expr) : value =
  match e with
  (*
    Case 1: Lambda abstraction -> λx.b where b is the body of the function
  *)
  | Abs (x, body) ->
    (*
      Return a value, with the environment specified by the function call, the
      expression's variable, and the body of the expression
    *)
    Clos { env = rho; var = x; body = body }
  
  (*
    Case 2: A variable, if e is a variable that said. Otherwise report an error.
  *)
  | Var x ->
  begin
    match lookup rho x with
    | Some v -> v
    | None -> failwith (Printf.sprintf "Unknown variable %s" x)
  end

  (*
    Case 3: Functional application -> (rator rand)

    (rator is the operator and the rand is the operand)
  *)
  | App (e_rator, e_rand) ->
    (* Evaluate the function (rator), to get the function's value e_rator *)
    let v_rator = eval rho e_rator in
    (* Evaluate the argument (rand), to get the argument's value e_rand *)
    let v_rand =  eval rho e_rand in
    (* Finally apply the function. *)
    apply v_rator v_rand

and apply (clos : value) (arg : value) : value =
  match clos with
  (*
    - Deconstructs the record Clos as 'c'.
    - Creates the new environment with the capture environment, the variable and
      the specified argument.
    - Finally evaluates the new environment and the body of the closure.
  *)
  | Clos c -> let new_env = extend c.env c.var arg in
  eval new_env c.body

(* Pretty printers for expressions and values *)
let rec string_of_expr = function
  | Var s -> s
  | Abs (s, e) -> "(λ" ^ s ^ ". " ^ string_of_expr e ^ ")"
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"

let string_of_value = function
  | Clos { var; body; _ } ->
    "Closure(λ" ^ var ^ ". " ^ string_of_expr body ^ ")"

let initial_env = []