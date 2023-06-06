open Lqccs

exception Fatal_error of string

(* let[@inline] ( >> ) f g x = g (f x) *)

(** [load_file filename] Read the file and return the file content as a string. *)
let load_file filename =
  try
    let ic = open_in filename in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s
  with Sys_error msg -> raise (Fatal_error msg)

let parse source =
  let lexbuf = Lexing.from_string ~with_positions:true source in
  Parsing.parse Scanner.next_token lexbuf

let eval source =
  try 
    parse source 
    |> Typechecker.typecheck 
    |> Eval.eval; 
    print_endline "OK" (* TODO: print result list from eval *)
  with
  | Scanner.Lexing_error (pos, msg) | Parsing.Syntax_error (pos, msg) ->
      let header = Printf.sprintf "Parsing error:" in
      Errors.report_singleline header source pos msg;
      Stdlib.flush_all ()
  | Typechecker.TypeException (msg, pos) ->
      let header = Printf.sprintf "Typechecking error:" in
      Errors.report_multiline header source pos msg;
      Stdlib.flush_all ()

let rec repl source =
  Printf.printf "> ";
  Stdlib.flush_all ();
  source := read_line ();
  if !source = "quit" then exit 0 else 
    eval !source;
    repl source

let () =
  try
    let source = ref "" in
    let sourcefile = ref "" in
    let usage =
      Printf.sprintf "Usage:\t%s [options] <source_file>\n" Sys.argv.(0)
    in
    Arg.parse [] (fun file -> sourcefile := file) usage;
    try
      if !sourcefile = "" then
        let _ =
          Printf.printf "\nREPL loop. To exit send \"quit\".\n";
          Stdlib.flush_all ()
        in
        repl source
      else source := load_file !sourcefile;
      eval !source
    with
    | Scanner.Lexing_error (pos, msg) | Parsing.Syntax_error (pos, msg) ->
        let header = Printf.sprintf "Parsing error:" in
        Errors.report_singleline header !source pos msg;
        Stdlib.flush_all ()
    | Typechecker.TypeException (msg, pos) ->
        let header = Printf.sprintf "Typechecking error:" in
        Errors.report_multiline header !source pos msg;
        Stdlib.flush_all ()
  with
  | Fatal_error msg | Failure msg ->
      Printf.eprintf "\027[1;31mFatal error:\027[0m %s\n" msg
  | _ -> Printf.eprintf "\027[1;31mUnexpected error\027[0m\n"
