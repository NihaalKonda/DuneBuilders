val scenarios : Scenariohandler.scenario list
(** [scenarios] is the list of type [Scenariohandler.scenario] describing the
    different situations that the user will have to go through for traffic cop
    game. *)

val play_traffic_cop : unit -> int
(** [play_traffic_cop] allows the user to play the traffic cop game and returns
    an int [points] *)
