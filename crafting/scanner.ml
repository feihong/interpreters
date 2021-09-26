#mod_use "error.ml"

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
    | None -> if t.literal = Eof then "EOF" else t.lexeme
    | Some (type', value) -> Printf.sprintf "%s %s: %s" type' t.lexeme value
end
(* Token *)

type t = {
  source: string;
  mutable tokens: Token.t list;
  mutable start: int;
  mutable current: int;
  mutable line: int;
}

let make source = {source; tokens = []; start = 0; current = 0; line = 1}

let is_at_end {current; source; _} = current >= String.length source

let advance ({source; current; _} as t) =
  let c = String.get source current in
  t.current <- current + 1;
  c

let add_token ({source; start; current; line; tokens} as t) literal =
  let lexeme = String.sub source start (current - start) in
  let token = Token.make literal lexeme line in
  t.tokens <- token :: tokens

let match' ({source; current; _} as t) expected =
  if is_at_end t then
    false
  else if String.get source current <> expected then
    false
  else
    let () = t.current <- current + 1 in
    true

let scan_token ({current; line; _} as t) =
  let c = advance t in
  match c with
  | '(' -> add_token t LParen
  | ')' -> add_token t RParen
  | '{' -> add_token t LBrace
  | '}' -> add_token t RBrace
  | ',' -> add_token t Comma
  | '.' -> add_token t Dot
  | '-' -> add_token t Minus
  | '+' -> add_token t Plus
  | ';' -> add_token t Semicolon
  | '*' -> add_token t Star
  | c ->
    Error.error line (Printf.sprintf "Unexpected character '%c'" c)

let scan_tokens t =
  while not (is_at_end t) do
    (* we are at the beginning of the next lexeme *)
    t.start <- t.current;
    scan_token t
  done;
  let eof_token = Token.make Eof "" t.line in
  t.tokens <- List.rev (eof_token :: t.tokens);
  t.tokens
