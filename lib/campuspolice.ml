open Types

(* Define Campus Police scenarios *)
let campus_police_scenarios = [
  {
    title = "Noise Complaint";
    description = "Late at night, noise complaint. Students partying loudly.";
    options = ["Warn them"; "Issue citation"; "Call backup"]
  };
  {
    title = "Lost Property";
    description = "A student lost their wallet near the library.";
    options = ["Search area"; "Ignore"; "Contact security"]
  }
]

let get_campus_police_scenarios () = campus_police_scenarios
