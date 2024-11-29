(* Type representing the game states *)
type game_state = Welcome | RoleSelection | Scenario of Role.scenario

(* Initialize the GUI window *)
val initialize_gui : unit -> unit

(* Main game loop to manage transitions between states *)
val game_loop : game_state -> unit
