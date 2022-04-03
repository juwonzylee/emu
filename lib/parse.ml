(* Parsing json input file ----------------------------------------- *)

type yojson = Yojson.Safe.t
let yojson_of_yojson = Fun.id
let pp_yojson = Yojson.Safe.pretty_print

type chord_t = {
  root : yojson;[@key "chordRoot"]
  shorthand : yojson;[@key "chordShorthand"]
  duration : int
} [@@deriving yojson, show] [@@yojson.allow_extra_fields]

type content_t = {
  tag : string option;[@yojson.option]
  contents : yojson list option;[@yojson.option]
  status : yojson option;[@yojson.option]
  root : yojson option;[@yojson.option]
  chords : chord_t list option [@yojson.option]
} [@@deriving yojson, show] [@@yojson.allow_extra_fields]

type label_t = {
  tag : yojson;
  contents : content_t
} [@@deriving yojson, show] [@@yojson.allow_extra_fields]

type t = {
  children : t list ;
  pn : yojson ;
  label : label_t
} [@@deriving yojson, show] [@@yojson.allow_extra_fields]


(* Reading file input *)
(* let file = IO.with_in "./output/example.json" IO.read_all *)

(* let parse () = 
  Yojson.Safe.from_string file |> of_yojson

let main () = 
  let harmtrace = Yojson.Safe.from_string file in
  print_endline @@ yojsono_string harmtrace *)