let simple_report header msg =
  Printf.eprintf "\027[1;31m%s\027[0m: \027[1m%s\027[0m\n"
  header msg

let report_line header line (lexeme_pos: Location.lexeme_pos) msg =
  try
    let prefix = String.make (lexeme_pos.Location.start_column - 1) ' ' in
    let arrows = String.make
      (lexeme_pos.Location.end_column - lexeme_pos.Location.start_column + 1)
      '^'
    in
    Printf.eprintf "\027[1;31m%s\027[0m, File %s, line %d: \027[1m%s\027[0m\n%s\n%s\027[1;31m%s\027[0m\n"
    header lexeme_pos.Location.filename lexeme_pos.Location.line msg 
    line 
    prefix arrows
  with _ -> simple_report header msg

let report_singleline header source lexeme_pos msg =
  let lines = String.split_on_char '\n' source in
  let line = try List.nth_opt lines (lexeme_pos.Location.line - 1) with _ -> None in
  match line with 
    Some(content) -> report_line header content lexeme_pos msg 
  | None -> report_line header "" lexeme_pos msg
  
let report_multiline header source code_pos msg =
  try
    let lines = 
      String.split_on_char '\n' source
      |> List.filteri (fun line _ ->
            code_pos.Location.start_line - 1 <= line
            && line <= code_pos.Location.end_line - 1) in
    match lines with
      [] -> simple_report header msg
    | [line] -> 
      report_line header line Location.{ 
        filename = code_pos.Location.filename;
        line = code_pos.Location.start_line; 
        start_column = code_pos.Location.start_column; 
        end_column = code_pos.Location.end_column 
      } msg
    | all -> 
      let text = all |> List.filteri (fun i _ -> i < 5) |> String.concat "\n" in
      Printf.eprintf "\027[1;31m%s\027[0m, File %s, lines %d-%d: \027[1m%s\027[0m\n%s\n"
      header code_pos.Location.filename
      code_pos.Location.start_line
      (code_pos.Location.start_line + 5)
      msg
      text
  with _ -> simple_report header msg