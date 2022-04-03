(* Defines the OCaml types of the chord input *)

type root =
| C
| D
| E
| F
| G
| A
| B
[@@deriving sexp]

type accidental =
| Sharp
| Natural
| Flat 
[@@deriving sexp]

type shorthand = 
| Maj
| Min
[@@deriving sexp]

type keyRoman =
| G
| D
| A
| E
| B
| Fsharp
| Csharp
[@@deriving sexp]

type key = 
| Maj of keyRoman
| Min of keyRoman
[@@deriving sexp]

type extension = 
| Fifth
| Seventh
| Ninth
[@@deriving sexp]

type chord = {
  root : root;
  accidental : accidental;
  shorthand : shorthand;
  extension : extension
} [@@deriving sexp]
let chord_of_sexp = function
| Sexplib.Sexp.List[Atom "Chord"; List[root; accidental; shorthand; extension]] ->
  {root=root_of_sexp root; accidental=accidental_of_sexp accidental;
  shorthand=shorthand_of_sexp shorthand; extension=extension_of_sexp extension}
| _ -> assert(false)

let sexp_of_chord {root; accidental; shorthand; extension} = 
  let open Sexplib.Sexp in
  List[Atom "Chord"; List[sexp_of_root root; sexp_of_accidental accidental; sexp_of_shorthand shorthand; sexp_of_extension extension]]

let rec list_of_sexp f = function 
| Sexplib.Sexp.Atom "[]" -> []
| List[Atom "::"; h; t] -> f h :: list_of_sexp f t
| _ -> assert(false)

let rec sexp_of_list f = 
  let open Sexplib.Sexp in
  function | [] -> Atom "[]" | h::tl -> List[Atom "::"; f h; sexp_of_list f tl]

let x = Sexplib.Std.sexp_of_list

type phrase =
| Ton of chord list
| Dom of chord list
| DomPhrase of phrase * phrase
| Sub of chord list
[@@deriving sexp]

type piece = phrase list [@@deriving sexp]