type role =
  | CampusPolice
  | CriminalInvestigator
  | TrafficCop

type scenario = {
  title : string;
  description : string;
  options : string list;
}

let get_scenario role =
  match role with
  | CampusPolice ->
      {
        title = "Campus Police Scenario";
        description =
          "It's late at night, and you receive a noise complaint near campus.\n\
           You see students partying and drinking.";
        options =
          [
            "Knock on the door and request to speak with the residents";
            "Call for backup before approaching";
            "Issue noise violation citations immediately";
          ];
      }
  | CriminalInvestigator ->
      {
        title = "Criminal Investigator Scenario";
        description =
          "A jewelry store has been robbed. The front door is damaged,\n\
           and display cases are shattered.";
        options =
          [
            "Interview the store owner";
            "Secure the crime scene and call for forensics";
            "Check nearby security footage";
          ];
      }
  | TrafficCop ->
      {
        title = "Traffic Cop Scenario";
        description =
          "You pull over a car for running a red light.\n\
           The driver seems nervous and is fidgeting.";
        options =
          [
            "Request license and registration calmly";
            "Call for backup immediately";
            "Order the driver to exit the vehicle";
          ];
      }

let handle_role_selection x y =
  if x >= (600 / 2) - 100 && x <= (600 / 2) + 100 then
    if y >= (400 / 2) + 50 && y <= (400 / 2) + 100 then Some CampusPolice
    else if y >= (400 / 2) - 20 && y <= (400 / 2) + 30 then
      Some CriminalInvestigator
    else if y >= (400 / 2) - 90 && y <= (400 / 2) - 40 then Some TrafficCop
    else None
  else None
