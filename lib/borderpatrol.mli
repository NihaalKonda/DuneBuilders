val scenarios : Scenariohandler.scenario list
(** [scenarios] is the list of type [Scenariohandler.scenario] that describes
    the different situations that the user will have to go through in the border
    patrol game. *)

val play_border_patrol : unit -> int
(** [play_border_patrol] allows the user to play the border patrol game and
    returns an int [points] *)
