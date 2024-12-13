val scenarios : Scenariohandler.scenario list
(** [scenarios] is the list of type [Scenariohandler.scenario] describing the
    different situations that the user will have to go through for campus police
    game. *)

val play_campus_police : unit -> int
(** [play_campus_police] allows the user to play the campus police game and
    returns an int [points] *)
