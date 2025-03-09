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
  let standalone =
    List.filter (fun (name, extends, declarations) -> extends == None) p
  in
  let rec assert_unique (p : Ast.pfile) =
    match p with
    | [] -> ()
    | (this, _, _) :: others -> (
        try
          let { loc; _ }, _, _ =
            List.find (fun (other, _, _) -> this.id = other.id) others
          in
          error ~loc "class %s already defined" this.id
        with Not_found -> assert_unique others)
  in
  failwith "TODO"
