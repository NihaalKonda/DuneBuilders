(* Type to represent roles *)
type role = CampusPolice | CriminalInvestigator | TrafficCop

(* Handle role selection based on mouse coordinates *)
val handle_role_selection : int -> int -> role option

(* Display the message for a selected role *)
val display_role_message : role -> unit
