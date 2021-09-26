let read_file f =
  let in_channel = open_in f in
  let size = in_channel_length in_channel in
  let bytes = Bytes.create size in
  really_input in_channel bytes 0 size;
  close_in in_channel;
  Bytes.unsafe_to_string bytes

let had_error = ref(false)

let report line where message =
  Printf.eprintf "[line %d] Error%s: %s" line where message;
  had_error := true

let error line message =
  report line "" message

let run s =
  print_endline s;

let run_file file =
  run (read_file file);
  if !had_error then exit 65

let run_prompt () =
  try
    while true do
      print_string "> ";
      run (read_line ())
      had_error := false
    done
with
    End_of_file -> print_endline "\nrun_prompt"

let main () =
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 -> run_file Sys.argv.(1)
  | _ -> print_endline "Usage: lox [script]"

let _ = main ()
