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
  List.of_seq
    (Seq.map (class_from_string p classes) (Hashtbl.to_seq_keys classes))
