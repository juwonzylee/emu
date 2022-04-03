open Ego.Basic
open Chord
open Containers

(* Deriving sexp for e-graph input ---------------------------------- *)
let sexp = 
  (module struct
    type t = Sexplib.Sexp.t
    let pp = Sexplib.Sexp.pp_hum
    let equal = Sexplib.Sexp.equal
  end : Alcotest.TESTABLE with type t = Sexplib.Sexp.t)

(* Encoding functional harmony rules -------------------------------- *)

type harmony_function =
| Dominant
| Tonic
| Subdominant
| Other

(* chord_function: categorizes the input chord c as one of four:
    1. Tonic
    2. Subdominant
    3. Dominant
    4. Other *)
let chord_function (c: chord) : harmony_function =
  begin match c with
  | Chord.{root=C; accidental=_; shorthand=Maj; extension=_} -> Tonic
  | Chord.{root=D; accidental=_; shorthand=Min; extension=_} -> Subdominant
  | Chord.{root=E; accidental=_; shorthand=Min; extension=_} -> Tonic
  | Chord.{root=F; accidental=_; shorthand=Maj; extension=_} -> Subdominant
  | Chord.{root=G; accidental=_; shorthand=Maj; extension=_} -> Dominant
  | Chord.{root=A; accidental=_; shorthand=Min; extension=_} -> Subdominant
  | Chord.{root=B; accidental=_; shorthand=Min; extension=_} -> Dominant
  | _ -> Other
  end

let rewrite_chords (c: chord) : Rule.t list =
  let c_fn = chord_function c in
  let root, accidental, shorthand, _extension = 
    match c with
    | {root=r; accidental=a; shorthand=s; extension=e} -> r,a,s,e in
  let from = Query.of_sexp (Chord.sexp_of_chord c) in

  match c_fn with
  | Other -> []
  | Tonic -> 
    (* Rule1: Extension chord *)
    let tonic_into1 = Query.of_sexp (Chord.sexp_of_chord {root=root; accidental=accidental; shorthand=shorthand; extension=Seventh}) in
    [Option.get_exn_or "error" @@ Rule.make ~from:from ~into:tonic_into1]

  | Subdominant -> 
    (* Rule1: Extension chord *)
    let sub_into1 = Query.of_sexp (Chord.sexp_of_chord {root=root; accidental=accidental; shorthand=shorthand; extension=Seventh}) in
    (* Rule2: Passing chords 2-5-1 *)
    let sub_into2 = Query.of_sexp (Chord.sexp_of_phrase Chord.(Sub([{root=G;accidental=Natural;shorthand=Min;extension=Fifth}; 
                                                                    {root=C; accidental=Natural; shorthand=Maj; extension=Fifth};
                                                                    {root=F; accidental=Natural; shorthand=Maj; extension=Fifth}]))) in
    (* Rule3: Subdominant followed by secondary dominant *)
    let sub_into3 = Query.of_sexp (Chord.sexp_of_phrase Chord.(Sub([{root=E;accidental=Natural;shorthand=Min;extension=Fifth}; 
                                                                    {root=A; accidental=Natural; shorthand=Min; extension=Fifth}]))) in
    (* Rule4: Functional harmony rules *)
    let sub_into4 = Query.of_sexp (Chord.sexp_of_chord {root=B; accidental=Flat; shorthand=Maj; extension=Fifth}) in
    [Option.get_exn_or "error" @@ Rule.make ~from:from ~into:sub_into1;
    Option.get_exn_or "error" @@ Rule.make ~from:from ~into:sub_into2;
    Option.get_exn_or "error" @@ Rule.make ~from:from ~into:sub_into3;
    Option.get_exn_or "error" @@ Rule.make ~from:from ~into:sub_into4]
  
  | Dominant -> 
    (* Rule1: Extension chord *)
    let dom_into1 = Query.of_sexp (Chord.sexp_of_chord {root=root; accidental=accidental; shorthand=shorthand; extension=Seventh}) in
    (* Rule2: Passing chords *)
    let dom_into2 = Query.of_sexp (Chord.sexp_of_phrase Chord.(Sub([{root=C;accidental=Natural;shorthand=Maj;extension=Fifth}; 
                                                                    {root=E; accidental=Natural; shorthand=Min; extension=Fifth};
                                                                    {root=G; accidental=Natural; shorthand=Maj; extension=Fifth}]))) in
    (* Rule3: Replacing chords *)
    (* let dom_into3 = Query.of_sexp (Chord.sexp_of_chord {root=root; accidental=accidental; shorthand=shorthand; extension=Seventh}) in *)
    (* Rule4: Functional harmony *)
    let dom_into4 = Query.of_sexp (Chord.sexp_of_phrase Chord.(DomPhrase((Chord.(Sub([{root=G; accidental=Natural; shorthand=Min; extension=Fifth}])), 
                                                                          Chord.(Dom([{root=C;accidental=Natural;shorthand=Maj; extension=Fifth}])))))) in
    [Option.get_exn_or "error" @@ Rule.make ~from:from ~into:dom_into1; 
    Option.get_exn_or "error" @@ Rule.make ~from:from ~into:dom_into2;
    (* Option.get_exn_or "error" @@ Rule.make ~from:from ~into:dom_into3; *)
    Option.get_exn_or "error" @@ Rule.make ~from:from ~into:dom_into4]


(* generate_rewrite_rules: takes a chord and returns a list of rewrite rules
   based on functional harmony *)
let rec generate_rewrite_rules (p: phrase) : Rule.t list =
  match p with
  | DomPhrase (p1, p2) -> (generate_rewrite_rules p1)@(generate_rewrite_rules p2)
  | Ton ch_lst | Dom ch_lst | Sub ch_lst -> 
    let rules = List.fold_left (fun (acc:Rule.t list) c -> acc@(rewrite_chords c)) [] ch_lst in
    rules

(* Cost function based on emotions *)
let cost_function score (sym, children) =
  let node_score = 
    match Symbol.to_string sym with
    | "Min" -> 1.
    | "Maj" -> 0.
    | _ -> 0. in
  node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children

let saturate_and_extract (parsedInput: piece) =
  (* 1. Initialize e-graph *)
  let graph = EGraph.init () in

  (* 2. Extract rules and insert phrases into e-graph *)
  let rec acc_rules acc expr_id_list phrases = 
    match phrases with
    | [] -> acc, expr_id_list
    | h::tl -> 
      (* Insert phrase into graph and store id *)
      let id = EGraph.add_sexp graph (sexp_of_phrase h) in
      let tmp_rules = generate_rewrite_rules h 
      in acc_rules (acc@tmp_rules) (expr_id_list@[id]) tl
  in let rules, expr_ids = acc_rules [] [] parsedInput 
  in

  (* 3. Apply rewrite rules *)
  EGraph.apply_rules graph rules;
  (* EGraph.apply_rules graph rules; *)

  (* 4. Extract based on cost function *)
  let res = List.map (fun id -> 
                  let tmp_phrase = EGraph.extract cost_function graph id in
                  let phrase = Chord.phrase_of_sexp tmp_phrase in
                  Format.printf "e%d -> %a\n" (id :> int) Sexplib.Sexp.pp_hum tmp_phrase;
                  phrase
                  ) expr_ids in



  (* 5. Visualize extracted e-graph *)
  let g : Odot.graph = EGraph.to_dot graph in
  Chord.sexp_of_piece res |> Sexplib.Sexp.to_string_hum |> print_endline;
  print_endline @@ Odot.string_of_graph g
