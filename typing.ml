open Ast
open Class

let debug = ref false

let file ?(debug = !debug) (p : Ast.pfile) : Ast.tfile =
  Class.Assert.unique p;
  Class.Assert.no_string_inheritance p;

  let quick_look = Class.quick_look p in

  Class.Assert.valid_extends p quick_look;
  Class.Assert.acyclical p quick_look;

  let classes = Class.classes quick_look p in
  if debug then print_endline "Class tree built";

  failwith "TODO"
