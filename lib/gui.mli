(** The type representing the various states of the game. *)
type game_state =
  | Welcome  (** The initial state displaying the welcome message. *)
  | RoleSelection  (** The state where the user selects a role to play. *)
  | RoleSelected of string
      (** The state that displays the selected role before starting the game. *)
  | FinalScreen of (string * int) list
      (** The state displaying the final scores for all completed roles. *)

val window_width : int
(** Constants defining the GUI dimensions and button sizes. *)

val window_height : int
val button_width : int
val button_height : int

val draw_button : int -> int -> int -> int -> string -> unit
(** [draw_button x y width height text] draws a button at coordinates [(x, y)]
    with the specified [width] and [height] and the given [text]. *)

val display_welcome_message : unit -> unit
(** [display_welcome_message ()] displays the welcome screen with a "Start"
    button. *)

val display_role_selection : unit -> unit
(** [display_role_selection ()] displays the role selection screen with buttons
    for each available role. *)

val display_role_selected : string -> unit
(** [display_role_selected selected_role] displays a message indicating the
    [selected_role] chosen by the player before starting the game. *)

val display_final_screen : (string * int) list -> unit
(** [display_final_screen completed_roles] displays the final scores for all
    completed roles.
    - [completed_roles]: A list of tuples where each tuple contains a role name
      and the corresponding score. *)

val handle_role_click : int -> int -> string option
(** [handle_role_click x y] determines which role was clicked based on the mouse
    coordinates [(x, y)] and returns [Some role] if a role was clicked, or
    [None] otherwise. *)

val play_role_game : string -> string * int
(** [play_role_game role] runs the game for the given [role] and returns a tuple
    [(role, score)], where [role] is the name of the role played and [score] is
    the score obtained. *)

val game_loop : game_state -> (string * int) list -> unit
(** [game_loop state completed_roles] manages the transitions between different
    states of the game.
    - [state]: The current state of the game.
    - [completed_roles]: A list of roles that have been completed along with
      their scores. *)

val initialize_gui : unit -> unit
(** [initialize_gui ()] initializes the GUI window and starts the game loop in
    the [Welcome] state. *)
