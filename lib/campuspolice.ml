open Printf
open Sequence

(* Define a type for a scenario *)
type scenario = {
  description : string;
  options : (string * int * string) list; (* Option text, associated points, and explanation *)
  requires_sequence_game : bool; (* Does this scenario include the sequence game? *)
}

(* Define a list of scenarios for Campus Police *)
let scenarios = [
  {
    description = "It's late at night, and you receive a noise complaint near campus.\nYou see students partying and drinking.";
    options = [
      ("Knock on the door and request to speak with the residents", 2, "This is the best approach to de-escalate the situation while maintaining professionalism.");
      ("Call for backup before approaching", 1, "This is a safe option but might escalate the situation unnecessarily.");
      ("Issue noise violation citations immediately", -1, "This is too aggressive and may lead to confrontation.");
    ];
    requires_sequence_game = false;
  };
  {
    description = "A suspicious package is found in the dormitory lobby.\nYou need to decide your next steps.";
    options = [
      ("Call the bomb squad immediately", 2, "This ensures safety and gets professionals involved quickly.");
      ("Evacuate the dorm and secure the area", 1, "This is a good option but delays professional assessment.");
      ("Ignore the package and continue patrolling", -2, "This is dangerous and irresponsible.");
    ];
    requires_sequence_game = true; (* Trigger the sequence game *)
  };
  {
    description = "You spot an argument escalating into a fight near the campus cafeteria.";
    options = [
      ("Intervene and de-escalate the situation", 2, "This is the most effective way to prevent further escalation.");
      ("Call for backup and monitor the situation", 1, "This ensures safety but may delay resolution.");
      ("Ignore the situation and continue patrolling", -1, "This is neglectful and could result in harm.");
    ];
    requires_sequence_game = false;
  }
]

(* Display a single scenario and get the user's choice *)
let rec handle_scenario scenario points =
  printf "\n%s\n" scenario.description;
  List.iteri (fun i (option, _, _) -> printf "%d) %s\n" (i + 1) option) scenario.options;
  printf "Enter your choice: ";
  match read_line () with
  | exception End_of_file ->
      printf "No input received. Exiting game.\n";
      points (* Return current points *)
  | input -> (
      try
        let choice = int_of_string input in
        if choice < 1 || choice > List.length scenario.options then (
          printf "Choice out of range. No points awarded.\n";
          handle_scenario scenario points (* Retry current scenario *)
        ) else (
          let _, points_for_choice, explanation = List.nth scenario.options (choice - 1) in
          printf "\n%s\n" explanation;
         (* Compute updated points first *)
let updated_points = points + points_for_choice in

(* Handle the sequence game, if required *)
(* Check if this scenario triggers the sequence game *)
  if scenario.requires_sequence_game then (
    printf "\nIt's time to test your problem-solving skills with a sequence game!\n";
    Sequence.play_sequence_game (); (* Play the sequence game *)
  );
  
  

(* Return updated points after handling sequence game *)
updated_points

        )
      with
      | Failure _ ->
          printf "Invalid input. Please enter a valid number.\n";
          handle_scenario scenario points (* Retry current scenario *)
      | Invalid_argument _ ->
          printf "An error occurred. Please try again.\n";
          handle_scenario scenario points (* Retry current scenario *)
    )

(* Play the Campus Police game *)
let play_campus_police () =
  let rec play_scenarios scenarios points =
    match scenarios with
    | [] ->
        printf "\nGame over. Your final score is: %d\n" points
    | scenario :: remaining_scenarios ->
        let updated_points = handle_scenario scenario points in
        if updated_points <= 0 then
          printf "\nGame over. You lost all your points. Final score: %d\n" updated_points
        else
          play_scenarios remaining_scenarios updated_points
  in
  printf "Welcome to the Campus Police Game!\n";
  play_scenarios scenarios 5 (* Start with 5 points *)
