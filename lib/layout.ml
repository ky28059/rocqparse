open Vernacexpr
open Decls
open Constrexpr
open Names

let layout_vernac v =
  Pp.string_of_ppcmds @@ Ppvernac.pr_vernac v

let layout_lident (l : lident) =
  Id.to_string l.v

let layout_constr_expr (c : constr_expr) =
  match c.v with
  | _ -> "unknown_constr"

let layout_theorem_kind t =
  match t with
  | Theorem -> "Theorem"
  | Lemma -> "Lemma"
  | Fact -> "Fact"
  | Remark -> "Remark"
  | Property -> "Property"
  | Proposition -> "Proposition"
  | Corollary -> "Corollary"

let layout_pure p =
  match p with
  (* | VernacFixpoint _ -> "" *)
  | VernacStartTheoremProof (t, _) -> "(VernacStartTheoremProof (" ^ layout_theorem_kind t ^ ", ..." ^ "))"
  | VernacProof (None, None) -> "(VernacProof (None, None)) (* Proof *)"
  | VernacProof (None, Some _) -> "(VernacProof (None, Some ...)) (* Proof using *)"
  | VernacProof (Some _, None) -> "(VernacProof (Some ..., None)) (* Proof with *)"
  | VernacProof (Some _, Some _) -> "(VernacProof (Some ..., Some ...)) (* Proof with using *)"
  | VernacExactProof p -> "(VernacExactProof " ^ layout_constr_expr p ^ ")"
  | VernacEndProof Admitted -> "(VernacEndProof Admitted)"
  | VernacEndProof (Proved (Transparent, None)) -> "(VernacEndProof (Proved (Transparent, None))) (* Defined *)"
  | VernacEndProof (Proved (Opaque, None)) -> "(VernacEndProof (Proved (Opaque, None))) (* Qed *)"
  | VernacEndProof (Proved (Opaque, Some l)) -> "(VernacEndProof (Proved (Opaque, Some " ^ layout_lident l ^ ")))"
  | _ -> "unknown_pure"

let layout_expr expr =
  match expr with
  | VernacSynPure p -> "(VernacSynPure " ^ layout_pure p ^ ")"
  | VernacSynterp _ -> "unknown_synterp"

let layout_vernac_ast ({ v = { expr; _ }; _ } : vernac_control) =
  layout_expr expr
