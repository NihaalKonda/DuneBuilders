type scenario = {
  description : string;
  options : (string * int) list;
}

val play_scenario : scenario -> int
val calculate_points : scenario list -> int -> int
val play_criminal_investigator : unit -> unit
