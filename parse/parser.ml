let parse_all_ast parsable =
  (* let mode = Ltac_plugin.G_ltac.classic_proof_mode in *)
  let entry = Pvernac.main_entry None in
  let rec f parser =
    match Procq.Entry.parse entry parser with
    | None -> []
    | Some ast -> ast :: f parser
  in
  f parsable
