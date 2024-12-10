type scenario = {
  description : string;
  options : (string * int * string) list;
      (* Option text, points, and explanation *)
  requires_sequence_game : bool;
  requires_reaction_game : bool;
  requires_scramble_game : bool;
  requires_math_game : bool;
}

val play_border_patrol : unit -> unit
