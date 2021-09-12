(* Eva interpreter from Essentials of Interpretation course
   http://dmitrysoshnikov.com/courses/essentials-of-interpretation/
*)

#mod_use "sexp.ml"

exception EvalError of string

module SMap = Map.Make(String)

let errorf fmt = Printf.ksprintf (fun s -> raise (EvalError s)) fmt

let show = Sexp.show

type env = {
  map: Sexp.t SMap.t;
  parent: env option;
}

let make_env parent = {map = SMap.empty; parent }

let rec eval (env: env) (expr : Sexp.t)  =
  match expr with
  | (String _ | Number _) as atom -> env, atom
  | Symbol name ->
    (match SMap.find_opt name env.map with
    | None -> errorf "Unrecognized variable: %s" name
    | Some value -> env, value)
  | List [Symbol "var"; Symbol name; e] ->
    let _, value = eval env e in
    { env with map = SMap.add name value env.map }, value
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

let eval' s =
  let folder (env, _value) exp = eval env exp in
  let (_env, value) = Sexp.parse s |> List.fold_left folder (make_env None, Number 0.) in
  value

let cases = let open Sexp in [
  "101", Number 101.;
  {|"foo is bar"|}, String "foo is bar";
  "(+ 1 5)", Number 6.;
  "(- (* 3 5) (/ 72 12))", Number 9.;
  "(var foo (+ 44 55))", Number 99.;
  {|(var foo 3) (+ foo 5)|}, Number 8.;
]

let () = cases |> List.iter (fun (s, value) -> assert (eval' s = value))
