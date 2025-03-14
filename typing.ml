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

    let classes =
      let tree : (string, string option) Hashtbl.t =
        Hashtbl.create (List.length p)
      in
      List.iter
        (fun (this, extends, _) ->
          Hashtbl.add tree this.id
            (match extends with None -> None | Some cls -> Some cls.id))
        p;
      tree
    in

    let assert_valid_extends (classes : (string, string option) Hashtbl.t) =
      Hashtbl.iter
        (fun this extends ->
          match extends with
          | Some cls ->
              if not (Hashtbl.mem classes cls) then
                let { loc }, _, _ = pclass_from_string this in
                error ~loc "Invalid extends : superclass %s is never defined"
                  this
          | _ -> ())
        classes
    in
    assert_valid_extends classes;

    let assert_acyclical (classes : (string, string option) Hashtbl.t) =
      let in_cycle = Hashtbl.create (Hashtbl.length classes) in
      (* Hashtbl.iter (fun k _ -> Hashtbl.add in_cycle k false) classes; *)

      let rec is_cycle (turtle : string) (hare : string) =
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
                      let new_in_cycle = is_cycle turtle hare in
                      Hashtbl.add in_cycle turtle new_in_cycle;
                      Hashtbl.add in_cycle hare new_in_cycle;
                      new_in_cycle
                  with Not_found ->
                    let new_in_cycle = is_cycle turtle hare in
                    Hashtbl.add in_cycle turtle new_in_cycle;
                    Hashtbl.add in_cycle hare new_in_cycle;
                    new_in_cycle))
      in

      Hashtbl.iter
        (fun this _ ->
          if is_cycle this this then
            let { loc }, _, _ = pclass_from_string this in
            error ~loc "%s is in cyclic dependency" this)
        classes
    in
    assert_acyclical classes;

    (** @returns (number of methods, number of attributes)*)
    let count_methods_attributes (decl : pdecl list) =
      List.fold_left
        (fun (m, a) declaration ->
          match declaration with
          | PDattribute _ -> (m, a + 1)
          | PDmethod _ -> (m + 1, a)
          | _ -> (m, a))
        (0, 0) decl
    in

    let rec class_from_string (cls : string) : class_ =
      let (this,parent,declarations) = pclass_from_string cls in
      let (m,a)=count_methods_attributes declarations in
          {
            class_name = cls;
            class_extends = (match (Hashtbl.find classes cls)  with
          | None -> None
          | Some parent -> Some (class_from_string parent));
          class_methods = Hashtbl.create m;
          class_attributes = Hashtbl.create a;
          }
    in

 let classes =  (Hashtbl.to_seq_keys classes) |> (Seq.map class_from_string) |> List.of_seq in

    failwith "TODO"
