open Ast

let debug = ref false
let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc = dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

let file ?debug:(b = false) (p : Ast.pfile) : Ast.tfile =
  debug := b;

  let pclass_from_string (cls : string) : pclass =
    List.find (fun (other, _, _) -> other.id = cls) p
  in

  let rec assert_unique (p : Ast.pfile) =
    match p with
    | [] -> ()
    | (this, _, _) :: others -> (
        try
          let { loc; _ }, _, _ = pclass_from_string this.id in
          error ~loc "class %s already defined" this.id
        with Not_found -> assert_unique others)
  in
  assert_unique p;

  try
    let { loc; id }, _, _ =
      List.find
        (fun (_, ext, _) ->
          match ext with Some ext -> ext.id = "String" | None -> false)
        p
    in
    error ~loc "%s inherits from String" id
  with Not_found ->
    ();
