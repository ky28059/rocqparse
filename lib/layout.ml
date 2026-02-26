open Vernacexpr
open Decls
open Constrexpr
open Proof_bullet
open Names
open Libnames

let layout_vernac v =
  Pp.string_of_ppcmds @@ Ppvernac.pr_vernac v

let layout_lident (l : lident) =
  Id.to_string l.v

let layout_qualid (q : qualid) =
  Id.to_string @@ qualid_basename q

let layout_ident_decl (lid, _l) =
  "(" ^ layout_lident lid ^ ", ...)"

let layout_constr_expr (c : constr_expr) =
  match c.v with
  | CRef (q, _) -> "(CRef (" ^ layout_qualid q ^ ", ...))"
  | CFix _ -> "(CFix ...)"
  | CCoFix _ -> "(CCoFix ...)"
  | CProdN _ -> "(CProdN ...)"
  | CLambdaN _ -> "(CLambdaN ...)"
  | CLetIn _ -> "(CLetIn ...)"
  | CAppExpl _ -> "(CAppExpl ...)"
  | CApp _ -> "(CApp ...)"
  | CProj _ -> "(CProj ...)"
  | CRecord _ -> "(CRecord ...)"
  | CCases _ -> "(CCases ...)"
  | CLetTuple _ -> "(CLetTuple ...)"
  | CIf _ -> "(CIf ...)"
  | CHole _ -> "(CHole ...)"
  | CGenarg _ -> "(CGenarg ...)"
  | CGenargGlob _ -> "(CGenargGlob ...)"
  | CPatVar _ -> "(CPatVar ...)"
  | CEvar _ -> "(CEvar ...)"
  | CSort _ -> "(CSort ...)"
  | CCast _ -> "(CCast ...)"
  | CNotation _ -> "(CNotation ...)"
  | CGeneralization _ -> "(CGeneralization ...)"
  | CPrim _ -> "(CPrim ...)"
  | CDelimiters _ -> "(CDelimiters ...)"
  | CArray _ -> "(CArray ...)"

let layout_proof_expr (id, (_, expr)) =
  "(" ^ layout_ident_decl id ^ ", (..., " ^ layout_constr_expr expr ^ "))"

let layout_theorem_kind t =
  match t with
  | Theorem -> "Theorem"
  | Lemma -> "Lemma"
  | Fact -> "Fact"
  | Remark -> "Remark"
  | Property -> "Property"
  | Proposition -> "Proposition"
  | Corollary -> "Corollary"

let layout_bullet b =
  match b with
  | Dash n -> "(Dash " ^ string_of_int n ^ ")"
  | Star n -> "(Star " ^ string_of_int n ^ ")"
  | Plus n -> "(Plus " ^ string_of_int n ^ ")"

let layout_pure p =
  match p with
  (* | VernacFixpoint _ -> "" *)
  | VernacStartTheoremProof (t, e) ->
    let p = String.concat ", " @@ List.map layout_proof_expr e in
    "(VernacStartTheoremProof (" ^ layout_theorem_kind t ^ ", [" ^ p ^ "]))"
  | VernacProof (None, None) -> "(VernacProof (None, None)) (* Proof *)"
  | VernacProof (None, Some _) -> "(VernacProof (None, Some ...)) (* Proof using *)"
  | VernacProof (Some _, None) -> "(VernacProof (Some ..., None)) (* Proof with *)"
  | VernacProof (Some _, Some _) -> "(VernacProof (Some ..., Some ...)) (* Proof with using *)"
  | VernacExactProof p -> "(VernacExactProof " ^ layout_constr_expr p ^ ")"
  | VernacEndProof Admitted -> "(VernacEndProof Admitted)"
  | VernacEndProof (Proved (Transparent, None)) -> "(VernacEndProof (Proved (Transparent, None))) (* Defined *)"
  | VernacEndProof (Proved (Opaque, None)) -> "(VernacEndProof (Proved (Opaque, None))) (* Qed *)"
  | VernacEndProof (Proved (Opaque, Some l)) -> "(VernacEndProof (Proved (Opaque, Some " ^ layout_lident l ^ ")))"
  | VernacBullet b -> "(VernacBullet " ^ layout_bullet b ^ ")"
  | _ -> "unknown_pure"

let layout_expr expr =
  match expr with
  | VernacSynPure p -> "(VernacSynPure " ^ layout_pure p ^ ")"
  | VernacSynterp _ -> "unknown_synterp"

let layout_vernac_ast ({ v = { expr; _ }; _ } : vernac_control) =
  layout_expr expr
