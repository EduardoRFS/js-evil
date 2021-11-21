module Flow_loc = struct
  module Loc = Loc
end
open Ppxlib
open Flow_loc

(* ppx logic *)
let parse_javascript code =
  (* TODO: loc source *)
  let code_ast, errors = Parser_flow.program code in
  (* TODO: use loc to show this errors *)
  assert (errors == []);
  code_ast

(* module Loc = struct
     include Loc
     type location = Location.t = {
       loc_start : position;
       loc_end : position;
       loc_ghost : bool;
     }

     and 'a t = 'a Loc.t = { txt : 'a; loc : location }

     and position = Lexing.position = {
       pos_fname : string;
       pos_lnum : int;
       pos_bol : int;
       pos_cnum : int;
     }
     [@@deriving show]
   end *)
let emit_javascript ~loc ast =
  let open Ast_builder.Make (struct
    let loc = loc
  end) in
  let ast_expr =
    ast
    |> Flow_ast.Program.show Loc.pp Loc.pp
    |> Lexing.from_string |> Parse.expression
  in

  [%expr
    let _, ast = [%e ast_expr] in
    ast]

let transform ~loc:_ ~path:_ code =
  let code_ast : (Loc.t, Loc.t) Flow_ast.Program.t =
    parse_javascript code.txt
  in
  emit_javascript ~loc:code.loc code_ast

(* driver *)
let string_constant_pattern =
  Ast_pattern.(
    pstr (pstr_eval (pexp_constant (pconst_string __' __ __)) __ ^:: nil)
    >>| fun f payload _ _ _ -> f payload)

let js_metaquot_rule =
  Extension.declare "js" Extension.Context.expression string_constant_pattern
    transform
  |> Context_free.Rule.extension

let () =
  Driver.register_transformation "js_metaquot" ~rules:[ js_metaquot_rule ]
