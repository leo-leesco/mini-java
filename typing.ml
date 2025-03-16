open Ast
open Class
open Assert

let debug = ref false

let file ?debug:(b = false) (p : Ast.pfile) : Ast.tfile =
  debug := b;

  Assert.unique p;
  Assert.no_string_inheritance p;

  let quick_look = Class.quick_look p in

  Assert.valid_extends p quick_look;
  Assert.acyclical p quick_look;

  let classes = Class.classes quick_look p in
  failwith "TODO"
