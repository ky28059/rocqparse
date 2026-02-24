let rec parse_all_ast parsable acc =
  try
    let ast_node = Procq.Entry.parse (Pvernac.main_entry None) parsable in
    parse_all_ast parsable (ast_node :: acc)
  with
  | Gramlib.Stream.Failure -> List.rev acc (* EOF *)
  | e -> raise e
