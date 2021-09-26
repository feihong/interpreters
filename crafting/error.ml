let had_error = ref(false)

let report line where message =
  Printf.eprintf "[line %d] Error%s: %s" line where message;
  had_error := true

let error line message = report line "" message
