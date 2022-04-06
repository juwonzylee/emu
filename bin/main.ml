
open Containers
open Lib.Chord
open Ego.Basic

(**--------------------------------------------------------------------
Pipeline:
  1. Run Harmtrace and output json file
  2. Parse json file into OCaml syntax
  3. Rewrite the individual chords based on encoded rules
  4. Extract based on cost function of emotion
  5. Output a pretty printed sequence of chords
*)

let example =
  [Ton([{root=E; accidental=Natural; shorthand=Min; extension=Fifth}])
  ;Sub([{root=F; accidental=Natural; shorthand=Maj; extension=Fifth}])
  ;Dom([{root=G; accidental=Natural; shorthand=Maj; extension=Fifth}])]

let dom_from1 = Query.of_sexp (sexp_of_chord {root=G; accidental=Natural; shorthand=Maj; extension=Fifth})
let dom_into1 = Query.of_sexp (sexp_of_chord {root=G; accidental=Natural; shorthand=Maj; extension=Seventh})

let ton_from1 = Query.of_sexp (sexp_of_chord {root=E; accidental=Natural; shorthand=Min; extension=Fifth})
let ton_into1 = Query.of_sexp (sexp_of_chord {root=C; accidental=Natural; shorthand=Maj; extension=Seventh})

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

  let expr_id = EGraph.add_sexp graph (sexp_of_piece example) in 

  let rule1 = Option.get_exn_or "none" (Rule.make ~from:dom_from1 ~into:dom_into1) in
  let rule2 = Option.get_exn_or "none" (Rule.make ~from:ton_from1 ~into:ton_into1) in
  let _ = EGraph.run_until_saturation graph [rule1; rule2] in

  let _result = EGraph.extract cost_function graph expr_id in


  let g : Odot.graph = EGraph.to_dot graph in
  (* result |> Sexplib.Sexp.to_string_hum |> print_endline; *)
  print_endline @@ Odot.string_of_graph g
