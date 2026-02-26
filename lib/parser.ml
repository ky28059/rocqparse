let parse_all_ast parsable =
  let rec f parser =
    let mode = Synterp.get_default_proof_mode () in
    let entry = Pvernac.main_entry (Some mode) in
    match Procq.Entry.parse entry parser with
    | None -> []
    | Some ast ->
      let _ = Synterp.synterp_control ~intern:Vernacinterp.fs_intern ast in
      ast :: f parser
  in
  f parsable
