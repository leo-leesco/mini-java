open Ast

let rec count_bindings (htb : ('a, 'b) Hashtbl.t) (key : 'a) : int =
  match Hashtbl.find_opt htb key with
  | None -> 0
  | Some value ->
      Hashtbl.remove htb key;
      let c = 1 + count_bindings htb key in
      Hashtbl.add htb key value;
      c

let () =
  let test = Hashtbl.create 10 in
  Hashtbl.add test 0 0;
  Hashtbl.add test 0 1;
  Hashtbl.add test 0 2;
  Hashtbl.add test 0 3;
  Hashtbl.add test 0 4;
  assert (count_bindings test 0 = 5);
  assert (Hashtbl.find test 0 = 4)

module Assert = struct
  let unique (class_tree : (string, class_) Hashtbl.t) (p : Ast.pfile) =
    Hashtbl.iter
      (fun name { class_name; class_extends; class_methods; class_attributes }
         -> ()) (* -> Hashtbl.iter (fun (methode methods) ->) *)
      class_tree

  (** if given attribute exists within attributes (meant to be the rest) or
      within parent class_
      @raise Error(attribute)*)
  let rec attr_not_exists (attribut : string)
      (attributes : (string, attribute) Hashtbl.t) (parent : class_ option) =
    match Hashtbl.find_opt attributes attribut with
    | None -> (
        match parent with
        | None -> ()
        | Some { class_extends; class_attributes } ->
            attr_not_exists attribut class_attributes class_extends)
    | Some _ ->
        Typing.error "attribute %s is already defined elsewhere" attribut

  (** if given meth exists within methods (meant to be the rest) or within
      parent class_
      @raise Error(meth)*)
  let rec attr_not_exists (methode : string)
      (methods : (string, method_) Hashtbl.t) (parent : class_ option) =
    match Hashtbl.find_opt methods methode with
    | None -> (
        match parent with
        | None -> ()
        | Some { class_extends; class_methods } ->
            attr_not_exists methode class_methods class_extends)
    | Some _ -> Typing.error "method %s is already defined elsewhere" methode
end
