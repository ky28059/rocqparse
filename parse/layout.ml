let layout_vernac v =
  Pp.string_of_ppcmds @@ Ppvernac.pr_vernac v
