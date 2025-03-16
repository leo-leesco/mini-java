open Ast

let debug = ref false

exception Error of Ast.location option * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id *)
let error ?(loc = None) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")
