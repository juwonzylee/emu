
open Containers
open Lib.Chord
(* open Lib.Rules *)
open Ego.Basic
(* open Sexplib0.Sexp  *)

(**--------------------------------------------------------------------
Pipeline:
  1. Run Harmtrace and output json file
  2. Parse json file into OCaml syntax
  3. Rewrite the individual chords based on encoded rules
  4. Extract based on cost function of emotion
  5. Output a pretty printed sequence of chords
*)


let () = Printexc.register_printer (function
| Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error(exn, t) -> 
  Some (Printexc.to_string exn ^ " in " ^ Yojson.Safe.pretty_to_string t) 
| _ -> None)
(* let () = Example.harmtrace_example() *)
(* 
let unwrap_error (arg:'a Ppx_deriving_yojson_runtime.error_or) : 'a = 
  match arg with 
  | Error e -> failwith e
  | Ok v -> v *)
(* let json = IO.read_all stdin
let () = 
  let raw_data = 
    json |> Yojson.Safe.from_string |> Lib.Parse.t_of_yojson in
  let piece = Lib.Extractor.extract raw_data in
  saturate_and_extract piece *)
  (* code to pretty print final piece like harmtrace input*)
  (* string_of_piece res |> print_endline  *)

  (* assert false *)


let example =
  [Ton([{root=E; accidental=Natural; shorthand=Min; extension=Fifth}])
  ;Sub([{root=F; accidental=Natural; shorthand=Maj; extension=Fifth}])
  ;Dom([{root=G; accidental=Natural; shorthand=Maj; extension=Fifth}])]

let dom_from1 = Query.of_sexp (sexp_of_chord {root=G; accidental=Natural; shorthand=Maj; extension=Fifth})
let dom_into1 = Query.of_sexp (sexp_of_chord {root=G; accidental=Natural; shorthand=Maj; extension=Seventh})

let ton_from1 = Query.of_sexp (sexp_of_chord {root=E; accidental=Natural; shorthand=Min; extension=Fifth})
let ton_into1 = Query.of_sexp (sexp_of_chord {root=C; accidental=Natural; shorthand=Maj; extension=Seventh})
(* let cost_function score (sym, children) =
  let node_score =
    match Symbol.to_string sym with
    | "*" -> 1. 
    | "/" -> 1. 
    | "<<" -> 2. 
    | _ -> 0. in
  node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children *)

let cost_function score (sym, children) =
  let node_score = 
    match Symbol.to_string sym with
    | "Min" -> 1.
    | "Maj" -> 0.
    | "Seventh" -> 0.
    | "Fifth" -> 1.
    | _ -> 0. in
  node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children
  
let () =
  let graph = EGraph.init () in

  (* let expr =
    List [Atom "/"; List[Atom "<<"; Atom "x"; Atom "1"]; Atom "2"] in

  let expr1 = 
    List[Atom "<<"; Atom "?x"; Atom "1"] in
  let expr2 = 
    List[Atom "*"; Atom "?x"; Atom "2"] in

  let expr3 = 
    List [Atom "/"; List[Atom "*"; Atom "?x"; Atom "2"]; Atom "2"] in
  let expr4 = 
    List[Atom "?x"] in *)

  let expr_id = EGraph.add_sexp graph (sexp_of_piece example) in 

  (* let _expr_id = EGraph.add_sexp graph expr in
  let from = Query.of_sexp expr1 in
  let into = Query.of_sexp expr2 in
  let from1 = Query.of_sexp expr3 in
  let into1 = Query.of_sexp expr4 in *)

  let rule1 = Option.get_exn_or "none" (Rule.make ~from:dom_from1 ~into:dom_into1) in
  let rule2 = Option.get_exn_or "none" (Rule.make ~from:ton_from1 ~into:ton_into1) in
  let _ = EGraph.run_until_saturation graph [rule1; rule2] in

  let result = EGraph.extract cost_function graph expr_id in

  (* let res_graph = EGraph.init () in *)
  (* let _ = EGraph.add_sexp graph example in  *)
  (* let g : Odot.graph = EGraph.to_dot graph in
  print_endline @@ Odot.string_of_graph g *)

  result |> Sexplib.Sexp.to_string_hum |> print_endline;


(* let moon_river =
  [Ton([{root=C; accidental=Natural; shorthand=Maj; extension=Fifth}])
  ; Sub([{root=D; accidental=Natural; shorthand=Min; extension=Fifth}])
  ; Dom([{root=F; accidental=Natural; shorthand=Maj; extension=Fifth}])
  ; Ton([{root=C; accidental=Natural; shorthand=Maj; extension=Fifth}])]
  (* ;Sub([{root=A; accidental=Natural; shorthand=Min; extension=Fifth}; 
      {root=F; accidental=Natural; shorthand=Maj; extension=Fifth};
      {root=E; accidental=Natural; shorthand=Maj; extension=Seventh}])] *)

let () =
  let _ = saturate_and_extract moon_river in
  () *)