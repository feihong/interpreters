(* Eva interpreter from Essentials of Interpretation course
   http://dmitrysoshnikov.com/courses/essentials-of-interpretation/
*)

#mod_use "sexp.ml"

exception EvalError of string

let errorf fmt = Printf.ksprintf (fun s -> raise (EvalError s)) fmt

let eval (expr : Sexp.t) =
  match expr with
  | Symbol _ as s -> s
  | String _ as s -> s
  | Number _ as n -> n
  | e -> errorf "Invalid expression: %s" (Sexp.show e)

let cases = let open Sexp in [
  "101", Number 101.;
  {|"foo is bar"|}, String "foo is bar";
  "abc-123-MANIC", Symbol "abc-123-MANIC";
]

let () = cases |> List.iter (fun (s, value) ->
  let expr = s |> Sexp.parse |> List.hd in
  assert (eval expr = value))
