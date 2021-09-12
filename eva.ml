(* Eva interpreter from Essentials of Interpretation course
   http://dmitrysoshnikov.com/courses/essentials-of-interpretation/
*)

#mod_use "sexp.ml"

exception EvalError of string

let errorf fmt = Printf.ksprintf (fun s -> raise (EvalError s)) fmt

let show = Sexp.show

let rec eval (expr : Sexp.t) =
  match expr with
  | Symbol _ as s -> s
  | String _ as s -> s
  | Number _ as n -> n
  | List [Symbol ("+" | "-" | "*" | "/" as opname); e1; e2] ->
    let op =
      match opname with
      | "+" -> (+.)
      | "-" -> (-.)
      | "*" -> ( *. )
      | "/" -> (/.)
      | _ -> errorf "Unrecognized binary operator: %s" opname
    in
    (match eval e1, eval e2 with
    | Number n1, Number n2 -> Number (op n1 n2)
    | Number _, e | e, Number _ -> errorf "%s operator didn't receive numeric operand: %s" opname (show e)
    | e1, e2  -> errorf "%s operator didn't receive numeric operands: %s and %s" opname (show e1) (show e2))
  | e -> errorf "Invalid expression: %s" (Sexp.show e)

let eval' s = s |> Sexp.parse |> List.hd |> eval

let cases = let open Sexp in [
  "101", Number 101.;
  {|"foo is bar"|}, String "foo is bar";
  "abc-123-MANIC", Symbol "abc-123-MANIC";
  "(+ 1 5)", Number 6.;
  "(- (* 3 5) (/ 72 12))", Number 9.;
]

let () = cases |> List.iter (fun (s, value) -> assert (eval' s = value))
