type scenario = {
  description : string;
  options : (string * int * string) list; (* Option text, points, and explanation *)
  requires_sequence_game : bool;
}

val play_campus_police : unit -> unit
