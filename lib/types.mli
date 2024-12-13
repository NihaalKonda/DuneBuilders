type role =
  | CampusPolice
  | CriminalInvestigator
  | TrafficCop
  | BorderPatrol
      (** Type representing a scenario with a title, description, and options *)

type scenario = {
  title : string;
  description : string;
  options : string list;
}
