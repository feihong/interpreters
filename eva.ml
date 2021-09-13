(* Eva interpreter from Essentials of Interpretation course
   http://dmitrysoshnikov.com/courses/essentials-of-interpretation/
*)

#mod_use "sexp.ml"

exception EvalError of string

let errorf fmt = Printf.ksprintf (fun s -> raise (EvalError s)) fmt

let show = Sexp.show

module Env = struct
  module SMap = Map.Make(String)

  type t = {
    map: Sexp.t SMap.t;
    parent: t option;
  }

  let make parent = { parent; map = SMap.empty }

  let rec find name env =
    match SMap.find_opt name env.map, env.parent with
    | (Some _) as value, _ -> value
    | None, None -> None
    | None, Some parent_env -> find name parent_env

  let define name value env = { env with map = SMap.add name value env.map }

  let rec set name value env =
    match SMap.mem name env.map, env.parent with
    | true, _ -> { env with map = SMap.add name value env.map }
    | false, None -> errorf "Can't set undeclared variable %s" name
    | false, Some parent_env -> { env with parent = Some (set name value parent_env) }
end

let rec eval (env: Env.t) (expr: Sexp.t)  =
  match expr with
  | (String _ | Number _) as atom -> env, atom
  | Symbol name ->
    (match Env.find name env with
    | None -> errorf "Unrecognized variable: %s" name
    | Some value -> env, value)
  | List [Symbol "var"; Symbol name; e] ->
    let _, value = eval env e in
    Env.define name value env, value
  | List [Symbol "set"; Symbol name; e] ->
    let _, value = eval env e in
    Env.set name value env, value
  | List (Symbol "begin" :: expr :: exprs) ->
    let block_env = Env.make (Some env) in
    let (block_env: Env.t), value = eval_multi block_env expr exprs in
    Option.get block_env.parent, value
  | List [Symbol ("+" | "-" | "*" | "/" as opname); e1; e2] ->
    let op =
      match opname with
      | "+" -> (+.)
      | "-" -> (-.)
      | "*" -> ( *. )
      | "/" -> (/.)
      | _ -> errorf "Unrecognized binary operator: %s" opname
    in
    (match eval env e1 |> snd, eval env e2 |> snd with
    | Number n1, Number n2 -> env, Number (op n1 n2)
    | Number _, e | e, Number _ -> errorf "%s operator didn't receive numeric operand: %s" opname (show e)
    | e1, e2  -> errorf "%s operator didn't receive numeric operands: %s and %s" opname (show e1) (show e2))
  | e -> errorf "Invalid expression: %s" (Sexp.show e)

and eval_multi (env: Env.t) (expr: Sexp.t) (exprs: Sexp.t list) =
  let init = eval env expr in
  List.fold_left (fun (env, _value) expr -> eval env expr) init exprs

let eval' s =
  match Sexp.parse s with
  | [] -> errorf "Nothing to evaluate"
  | expr :: exprs ->
    eval_multi (Env.make None) expr exprs |> snd

let cases = let open Sexp in [
  "101", Number 101.;
  {|"foo is bar"|}, String "foo is bar";
  "(+ 1 5)", Number 6.;
  "(- (* 3 5) (/ 72 12))", Number 9.;
  "(var foo (+ 44 55))", Number 99.;
  {|(var foo 3) foo|}, Number 3.;
  {|(var foo 3) (+ foo 5)|}, Number 8.;
  {|(var foo 3) (set foo 8) foo|}, Number 8.;
  {|(begin (- 100 1))|}, Number 99.;
  {|(var x 100)
    (begin (var x 111))
    x|}, Number 100.;
  {|(var x 100)
    (begin (set x 999))
    x|}, Number 999.;
]

let () = cases |> List.iter (fun (s, value) ->
  let computed = eval' s in
  if computed <> value then Printf.printf "%s evaluated to %s <> %s\n" s (show computed) (show value)
)
