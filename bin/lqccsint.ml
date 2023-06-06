open Lqccs
type action = Parse | Type_check | Interpret

exception Fatal_error of string

let[@inline] ( >> ) f g x = g (f x)

(** [load_file filename] Read the file and return the file content as a string. *)
let load_file filename =
  try 
    let ic = open_in filename in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s
  with Sys_error msg -> raise(Fatal_error(msg))

(** [parse filename] Parses lqccs source code. Returns the abstract 
    syntax tree. *)
let parse_file filename =
  let source = load_file filename in
  let lexbuf = Lexing.from_string ~with_positions:true source in
  Parsing.parse Scanner.next_token lexbuf

(*let parse_channel channel =
  let lexbuf = Lexing.from_channel ~with_positions:true channel in
  Parsing.parse Scanner.next_token lexbuf*)

let parse_stdin =
  let source = read_line () in
  let lexbuf = Lexing.from_string ~with_positions:true source in
  Parsing.parse Scanner.next_token lexbuf

let parse filename =
  if filename = "" then parse_stdin 
  else parse_file filename

let action_function = function
  | Parse -> 
    (* Parse given source code and print the abstract syntax tree *)
    parse >> Ast.show_program >> (Printf.printf "%s")
  | Type_check -> 
    parse >> Typechecker.typecheck >> ignore
  | Interpret -> 
    parse >> Eval.eval

let () =
  try
    let action = ref Interpret in
    let sourcefile = ref "" in
    let outputfile = ref "a.out" in
    let spec_list =
      [
        ("-p", 
          Arg.Unit (fun () -> action := Parse), "Parse and print AST");
        ( "-t",
          Arg.Unit (fun () -> action := Type_check),
          "Type checks and print the result" );
        ( "-i",
          Arg.Unit (fun () -> action := Interpret),
          "Interpret the source file" );
        ( "-o",
          Arg.Set_string outputfile,
          "Place the output into file (default: a.out)" );
      ]
    in
    let usage =
      Printf.sprintf "Usage:\t%s [options] <source_file>\n" Sys.argv.(0)
    in
    Arg.parse spec_list (fun file -> sourcefile := file) usage;
    try action_function !action !sourcefile with
			| Parsing.Syntax_error (pos, msg) | Scanner.Lexing_error (pos, msg) ->
        let source = try load_file !sourcefile with _ -> "" in
        Errors.report_singleline "Error" source pos msg
      | Typechecker.TypeException (msg, pos) | Eval.EvalException (pos, msg) ->
        let source = try load_file !sourcefile with _ -> "" in
        Errors.report_multiline "Error" source pos msg
  with 
    Fatal_error msg
  | Failure msg -> Printf.eprintf "\027[1;31mFatal error:\027[0m %s\n" msg
  | _ -> Printf.eprintf "\027[1;31mUnexpected error\027[0m\n" 