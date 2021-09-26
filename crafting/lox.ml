#mod_use "error.ml"
#mod_use "scanner.ml"

let read_file f =
  let in_channel = open_in f in
  let size = in_channel_length in_channel in
  let bytes = Bytes.create size in
  really_input in_channel bytes 0 size;
  close_in in_channel;
  Bytes.unsafe_to_string bytes

let run source =
  let scanner = Scanner.make source in
  let tokens = Scanner.scan_tokens scanner in
  tokens |> List.iter (fun token ->
    print_endline (Scanner.Token.show token)
  )

let run_file file =
  run (read_file file);
  if !Error.had_error then exit 65

let run_prompt () =
  try
    while true do
      print_string "> ";
      run (read_line ());
      Error.had_error := false
    done
with
    End_of_file -> print_endline "\nrun_prompt"

let main () =
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 -> run_file Sys.argv.(1)
  | _ -> print_endline "Usage: lox [script]"

let _ = main ()
