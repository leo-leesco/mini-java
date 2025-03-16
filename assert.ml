open Ast
(** Assert that the classes are well formed @raise Error(location, comment) *)

open Class

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

exception Error of Ast.location * string

(* use the following function to signal typing errors, e.g.
      error ~loc "unbound variable %s" id
*)
let error ?(loc = dummy_loc) f =
  Format.kasprintf (fun s -> raise (Error (loc, s))) ("@[" ^^ f ^^ "@]")

(**classes are uniquely defined*)
let rec unique (p : Ast.pfile) =
  match p with
  | [] -> ()
  | (this, _, _) :: others -> (
      try
        let { loc; _ }, _, _ = Class.pclass_from_string p this.id in
        error ~loc "class %s already defined" this.id
      with Not_found -> unique others)

(**Classes do not inherit from String*)
let no_string_inheritance (p : Ast.pfile) =
  match p with
  | [] -> ()
  | (this, extends, _) :: others -> (
      match extends with
      | Some extends when extends.id = "String" ->
          let loc = this.loc in
          error ~loc "%s inherits from String" this.id
      | _ -> ())

(**classes inherit from a class that is defined elsewhere*)
let valid_extends (p : Ast.pfile) (classes : (string, string option) Hashtbl.t)
    =
  Hashtbl.iter
    (fun this extends ->
      match extends with
      | Some cls ->
          if not (Hashtbl.mem classes cls) then
            let { loc }, _, _ = Class.pclass_from_string p this in
            error ~loc "Invalid extends : superclass %s is never defined" this
      | _ -> ())
    classes

let rec is_cycle (classes : (string, string option) Hashtbl.t)
    (in_cycle : (string, bool) Hashtbl.t) (turtle : string) (hare : string) =
  match Hashtbl.find classes hare with
  | None ->
      Hashtbl.add in_cycle hare false;
      false
  | Some parent_hare -> (
      match Hashtbl.find classes parent_hare with
      | None ->
          Hashtbl.add in_cycle hare false;
          false
      | Some hare -> (
          let turtle = Option.get (Hashtbl.find classes turtle) in
          if turtle = hare then (
            Hashtbl.add in_cycle turtle true;
            Hashtbl.add in_cycle hare true;
            true)
          else
            try
              let already_visited =
                Hashtbl.find in_cycle turtle || Hashtbl.find in_cycle hare
              in
              if already_visited then true
              else
                let new_in_cycle = is_cycle classes in_cycle turtle hare in
                Hashtbl.add in_cycle turtle new_in_cycle;
                Hashtbl.add in_cycle hare new_in_cycle;
                new_in_cycle
            with Not_found ->
              let new_in_cycle = is_cycle classes in_cycle turtle hare in
              Hashtbl.add in_cycle turtle new_in_cycle;
              Hashtbl.add in_cycle hare new_in_cycle;
              new_in_cycle))

(**there is no circular inheritance*)
let acyclical (p : Ast.pfile) (classes : (string, string option) Hashtbl.t) =
  let in_cycle = Hashtbl.create (Hashtbl.length classes) in

  Hashtbl.iter
    (fun this _ ->
      if is_cycle classes in_cycle this this then
        let { loc }, _, _ = Class.pclass_from_string p this in
        error ~loc "%s is in cyclic dependency" this)
    classes
