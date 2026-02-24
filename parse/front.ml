let parse_file (filepath : string) =
  let in_chan = open_in filepath in
  let parsable = Procq.Parsable.make (Gramlib.Stream.of_channel in_chan) in
  Parser.parse_all_ast parsable []

let parse_str (code: string) =
  let parsable = Procq.Parsable.make (Gramlib.Stream.of_string code) in
  Parser.parse_all_ast parsable []

let layout_vernac v =
  Pp.string_of_ppcmds @@ Ppvernac.pr_vernac v
