let rec parse_all_ast parsable acc =
  try
    let ast_node = Procq.Entry.parse (Pvernac.main_entry None) parsable in
    parse_all_ast parsable (ast_node :: acc)
  with
  | Gramlib.Stream.Failure -> List.rev acc (* EOF *)
  | e -> raise e

let parse_file (filepath : string) =
  let in_chan = open_in filepath in
  let parsable = Procq.Parsable.make (Gramlib.Stream.of_channel in_chan) in
  parse_all_ast parsable []

let layout_vernac v =
  Pp.string_of_ppcmds @@ Ppvernac.pr_vernac v

let () =
  let ast = parse_file "/home/ky28059/rocqparse/data/test2.v" in
  let filtered = List.filter_map (fun x -> x) ast in
  print_endline @@ String.concat "\n\n" @@ List.map layout_vernac filtered
