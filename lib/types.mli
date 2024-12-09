type role = CampusPolice | CriminalInvestigator | TrafficCop

(** Type representing a scenario with a title, description, and options *)
type scenario = {
  title : string;        (** The title of the scenario *)
  description : string;  (** A description of the scenario *)
  options : string list; (** A list of possible options for the scenario *)
}
