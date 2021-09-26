
module Token = struct
  type literal =
    (* single-character tokens *)
    | LParen
    | RParen
    | LBrace
    | RBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    (* one or two character tokens *)
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    (* literals *)
    | Identifier of string
    | String of string
    | Number of int
    (* keywords *)
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return
    | Super
    | This
    | True
    | Var
    | While
    (* eof *)
    | Eof

  type t = {
    literal: literal; (* combine type and literal fields *)
    lexeme: string;
    line: int;
  }

  let make literal lexeme line = {literal; lexeme; line}

  let show t =
    let meta =
      match t.literal with
      | Identifier s -> Some ("identifier", s)
      | String s -> Some ("string", s)
      | Number n -> Some ("number", string_of_int n)
      | _ -> None
    in
    match meta with
    | None -> t.lexeme
    | Some (type', value) -> Printf.sprintf "%s %s: %s" type' t.lexeme value
end
(* Token *)

type t = {
  source: string;
  tokens: Token.t list;
  start: int;
  current: int;
  line: int;
}

let make source = {source; tokens = []; start = 0; current = 0; line = 1}

let is_at_end {current; source; _} = current >= String.length source

let scan_token ({current; _} as t) =
  { t with current = current + 1 }

(* return scanner itself instead of just the tokens *)
let scan_tokens t =
  let rec loop t =
    match is_at_end t with
    | true ->
      let new_tokens = List.rev (Token.make Eof "" t.line :: t.tokens) in
      { t with tokens = new_tokens }
    | false ->
      (* we are at the beginning of the next lexeme *)
      let new_t = scan_token { t with start = t.current } in
      loop new_t
  in loop t
