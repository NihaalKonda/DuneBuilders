(** Abstraction Function: role is the role that represents a type of law
    enforcement personnel.
    - CampusPolice represents officers assigned to campus safety and security.
    - CriminalInvestigator represents personnel tasked with solving crimes.
    - TrafficCop represents officers who manage traffic and enforce road laws.
    - BorderPatrol represents personnel who secure borders and monitor
      cross-border activity. *)
type role =
  | CampusPolice
  | CriminalInvestigator
  | TrafficCop
  | BorderPatrol
      (** Type representing a scenario with a title, description, and options *)
      
(** AF: scenario is a scenario with a title, description, and a list of options:
    - title is the name or brief heading of the scenario.
    - description provides details about the scenario.
    - options lists the choices available to resolve or interact with the
      scenario. *)

type scenario = {
  title : string;
  description : string;
  options : string list;
}
