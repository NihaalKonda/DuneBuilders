(* Role types *)
type role = CampusPolice | CriminalInvestigator | TrafficCop

(* Scenario structure *)
type scenario = {
  title : string;
  description : string;
  options : string list;
}

(* Get the scenario for a specific role *)
val get_scenario : role -> scenario

(* Handle role selection based on mouse coordinates *)
val handle_role_selection : int -> int -> role option
