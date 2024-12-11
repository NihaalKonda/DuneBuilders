type scenario = {
  description : string;
  choices : (string * int * string) list;
    
  games_required : bool * bool * bool * bool;
   
}

val scenario_list : scenario list

val process_scenario : scenario -> int -> int
(** [process_scenario sc current_points] processes a single scenario [sc],
    updating the player's score [current_points] based on their choices. Returns
    the updated score. *)

val play_campus_police : unit -> int
(** [play_campus_police ()] starts and runs the Campus Police game. The game
    begins with a default score of 5 and returns the final score as an [int]
    after all scenarios are played or the game ends. *)
