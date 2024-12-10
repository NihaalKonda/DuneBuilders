(* criminalinvestigator.ml *)

(* Record type for a scenario *)
type scenario = {
  description : string;
  options : (string * int) list; (* Option description and associated points *)
}

(* Function to play a scenario and return the points awarded *)
let play_scenario scenario =
  print_endline scenario.description;
  List.iteri
    (fun i (desc, _) -> Printf.printf "%d. %s\n" (i + 1) desc)
    scenario.options;
  match read_int_opt () with
  | Some choice when choice >= 1 && choice <= List.length scenario.options ->
      let _, points = List.nth scenario.options (choice - 1) in
      Printf.printf "You chose: %s\n"
        (fst (List.nth scenario.options (choice - 1)));
      points
  | _ ->
      print_endline "Invalid choice. You get 0 points.";
      0

(* Function to keep track of points *)
let rec calculate_points scenarios total_points =
  match scenarios with
  | [] -> total_points
  | scenario :: rest ->
      let points = play_scenario scenario in
      calculate_points rest (total_points + points)

(* Main function to run the game *)
let play_criminal_investigator () =
  let scenarios =
    [
      {
        description =
          "Scenario 1: You arrive at the crime scene. What do you do?";
        options =
          [
            ("Examine the body.", 10);
            ("Question witnesses.", 8);
            ("Search the area for clues.", 7);
          ];
      };
      {
        description =
          "Scenario 2: A suspect is fleeing the scene. What do you do?";
        options =
          [
            ("Chase them on foot.", 6);
            ("Call for backup.", 10);
            ("Try to cut them off using your car.", 7);
          ];
      };
      {
        description =
          "Scenario 3: You found a mysterious object at the scene. What do you \
           do?";
        options =
          [
            ("Take it to the lab for analysis.", 10);
            ("Try to analyze it yourself.", 5);
            ("Leave it alone for now.", 2);
          ];
      };
      {
        description =
          "Scenario 4: A key witness is nervous and unwilling to talk. What do \
           you do?";
        options =
          [
            ("Offer them protection.", 10);
            ("Persuade them with evidence.", 8);
            ("Threaten to arrest them.", 3);
          ];
      };
      {
        description =
          "Scenario 5: You have gathered all the evidence. What do you do?";
        options =
          [
            ("Present it to the prosecutor.", 10);
            ("Review everything one more time.", 8);
            ("Confront the suspect directly.", 6);
          ];
      };
    ]
  in
  let total_points = calculate_points scenarios 0 in
  Printf.printf "Game over! Your total score is: %d\n" total_points
