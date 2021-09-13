(* Very basic S-expression parser *)
#mod_use "stream.ml"

type t =
  | Symbol of string
  | String of string
  | Number of float
  | Bool of bool
  | List of t list

let rec show t =
  match t with
  | Symbol s -> s
  | String s -> Printf.sprintf "%S" s
  | Number n -> string_of_float n
  | Bool b -> string_of_bool b
  | List l -> "(" ^ (l |> List.map show |> String.concat " ") ^ ")"

exception ParseError of string

exception EOF

let errorf fmt = Printf.ksprintf (fun s -> raise (ParseError s)) fmt

let is_space c = List.mem c ['\n'; '\t'; ' '; '\r']

let implode chars = chars |> List.map (String.make 1) |> String.concat ""

let reverse_implode chars = chars |> List.rev |> implode

let is_symbol c =
  match c with
  | '*' | '+' | '-' | '/' | '_' -> true
  | c when c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' -> true
  | _ -> false

let is_symbol_body c =
  match c with
  | c when is_symbol c -> true
  |'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let parse_number first_char stream =
  let rec loop acc =
    match Stream.peek stream with
    | Some ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' as c) ->
      Stream.junk stream;
      loop (c :: acc)
    | Some _ | None ->
      let s = reverse_implode acc in
      (match float_of_string_opt s with
      | None -> errorf "Invalid number: %s" s
      | Some n -> Number n)
  in loop [first_char]

let parse_symbol first_char stream =
  let rec loop acc =
    match Stream.peek stream with
    | Some c when is_symbol_body c ->
      Stream.junk stream;
      loop (c :: acc)
    | None | Some _ ->
      (match reverse_implode acc with
      | "true" -> Bool true
      | "false" -> Bool false
      | s -> Symbol s)
  in loop [first_char]

let parse_string stream =
  let rec loop acc =
    match Stream.next stream with
    | exception Stream.Failure -> errorf "Unterminated string: %s" (reverse_implode acc)
    | '"' -> String (reverse_implode acc)
    | '\\' ->
      (match Stream.next stream with
      | exception Stream.Failure -> errorf "Unterminated string: %s" (reverse_implode acc)
      | 'n' -> loop ('\n' :: acc)
      | 't' -> loop ('\t' :: acc)
      | 'r' -> loop ('\r' :: acc)
      | '"' -> loop ('\"' :: acc)
      | c -> errorf "Unrecognized escape sequence: \\%c" c
      )
    | c -> loop (c :: acc)
  in loop []

let rec parse_expr stream fn =
  match Stream.next stream with
  | exception Stream.Failure -> raise EOF
  | c when is_space c -> parse_expr stream fn
  | c when is_symbol c -> fn (parse_symbol c stream)
  | '-' | '.' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
    fn (parse_number c stream)
  | '"' -> fn (parse_string stream)
  | '(' -> parse_list stream [] fn
  | c -> errorf "Unexpected character: %c" c

and parse_list stream acc fn =
  match Stream.peek stream with
  | None -> errorf "Unterminated list"
  | Some c when is_space c ->
    Stream.junk stream;
    parse_list stream acc fn
  | Some ')' ->
    Stream.junk stream;
    fn (List (List.rev acc))
  | Some _ -> parse_expr stream (fun x -> parse_list stream (x :: acc) fn)

let parse s =
  let stream = Stream.of_string s in
  let rec loop acc =
    match parse_expr stream (fun x -> x) with
    | exception EOF -> List.rev acc
    | x -> loop (x :: acc)
  in loop []
