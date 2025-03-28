open Ast

let pclass_from_string (p : Ast.pfile) (cls : string) : pclass =
  List.find (fun (other, _, _) -> other.id = cls) p

(**build a Hashtbl to look up the parent class*)
let quick_look (p : Ast.pfile) =
  let tree : (string, string option) Hashtbl.t =
    Hashtbl.create (List.length p)
  in
  List.iter
    (fun (this, extends, _) ->
      Hashtbl.add tree this.id
        (match extends with None -> None | Some cls -> Some cls.id))
    p;
  tree

(** @return (number of methods, number of attributes) of a given pclass*)
let count_methods_attributes (cls : pclass) =
  let this, parent, decl = cls in
  List.fold_left
    (fun (m, a) declaration ->
      match declaration with
      | PDattribute _ -> (m, a + 1)
      | PDmethod _ -> (m + 1, a)
      | _ -> (m, a))
    (0, 0) decl

let rec class_from_string (p : Ast.pfile)
    (classes : (string, string option) Hashtbl.t) (cls : string) : class_ =
  let m, a = count_methods_attributes (pclass_from_string p cls) in
  {
    class_name = cls;
    class_extends =
      (match Hashtbl.find classes cls with
      | None -> None
      | Some parent -> Some (class_from_string p classes parent));
    class_methods = Hashtbl.create m;
    class_attributes = Hashtbl.create a;
  }

let classes (classes : (string, string option) Hashtbl.t) (p : Ast.pfile) =
  let class_tree = Hashtbl.create (Hashtbl.length classes) in
  Hashtbl.iter
    (fun cls _ -> Hashtbl.add class_tree cls (class_from_string p classes cls))
    classes;
  class_tree

(** Assert that the classes are well formed @raise Typing.Error(location, comment) *)
module Assert = struct
  (**classes are uniquely defined*)
  let unique (p : Ast.pfile) =
    let class_count = Hashtbl.create (List.length p) in
    List.iter
      (fun ({ loc; id }, _, _) ->
        match Hashtbl.find_opt class_count id with
        | None -> Hashtbl.add class_count id ()
        | Some () -> Typing.error ~loc:(Some loc) "class %s already defined" id)
      p

  (**Classes do not inherit from String*)
  let no_string_inheritance (p : Ast.pfile) =
    match p with
    | [] -> ()
    | (this, extends, _) :: others -> (
        match extends with
        | Some extends when extends.id = "String" ->
            let loc = Some this.loc in
            Typing.error ~loc "%s inherits from String" this.id
        | _ -> ())

  (**classes inherit from a class that is defined elsewhere*)
  let valid_extends (p : Ast.pfile)
      (classes : (string, string option) Hashtbl.t) =
    Hashtbl.iter
      (fun this extends ->
        match extends with
        | Some cls ->
            if not (Hashtbl.mem classes cls) then
              let { loc }, _, _ = pclass_from_string p this in
              Typing.error ~loc:(Some loc)
                "Invalid extends : superclass %s is never defined" this
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
          let { loc }, _, _ = pclass_from_string p this in
          Typing.error ~loc:(Some loc) "%s is in cyclic dependency" this)
      classes
end
