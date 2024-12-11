(* Type representing the state of the game *)
type game_state =
  | Welcome
  | RoleSelection
  | FinalScreen of (string * int) list

(* Initialize the GUI window *)
val initialize_gui : unit -> unit

(* Main game loop to manage transitions between states *)
val game_loop : game_state -> unit

(* Function to initialize the GUI *)
val initialize_gui : unit -> unit

(* Main game loop *)
val game_loop : game_state -> (string * int) list -> unit
