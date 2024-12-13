val scenarios : Scenariohandler.scenario list
(** [scenarios] is the list of type [Scenariohandler.scenario] describing the
    different situations that the user will have to go through for criminal
    investigator game. *)

val play_criminal_investigator : unit -> int
(** [play_criminal_investigator] allows the user to play the criminal
    investigator game and returns an int [points] *)
