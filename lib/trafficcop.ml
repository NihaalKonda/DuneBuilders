open Printf
open Scramble
open Sequence (* Assuming the sequence game is implemented in sequence.ml *)

(* Define a type for a scenario *)
type scenario = {
  description : string;
  options : (string * int * string) list; (* Option text, associated points, and explanation *)
  requires_scramble_game : bool; (* Does this scenario include the scramble game? *)
  requires_sequence_game : bool; (* Does this scenario include the sequence game? *)
}

(* Define a list of scenarios for Traffic Cop *)
let scenarios = [
  {
    description = "You receive a call about a driver weaving erratically through traffic. You need to act quickly.";
    options = [
      ("Pull the driver over for questioning", 2, "This is the safest and most responsible action.");
      ("Ignore the call and continue patrolling", -1, "Neglecting this can lead to a dangerous situation.");
      ("Request additional backup before proceeding", 1, "This ensures safety but delays action.");
    ];
    requires_scramble_game = false;
    requires_sequence_game = false; (* Trigger the sequence game *)
  };
  {
    description = "A car is parked illegally in a handicapped spot without a permit. While investigating, you find a clue that requires solving a scrambled word puzzle to verify parking records.";
    options = [
      ("Solve the puzzle to uncover more details", 2, "This ensures you investigate thoroughly and find the necessary information.");
      ("Ignore the puzzle and tow the car", -1, "This resolves the issue but might miss important information.");
      ("Call for help to solve the puzzle", 1, "This is cautious but slows down the investigation.");
    ];
    requires_scramble_game = true; (* Trigger the scramble game *)
    requires_sequence_game = false;
  };
  {
    description = "While monitoring traffic, you stop a vehicle with mismatched plates. Running the plates triggers a system alert requiring you to solve a pattern sequence to unlock detailed vehicle information.";
    options = [
      ("Solve the sequence to proceed", 2, "This helps you investigate the potential vehicle mismatch quickly.");
      ("Let the driver go with a warning", -1, "This neglects your duty to investigate properly.");
      ("Call for backup and delay the investigation", 1, "This ensures safety but slows down the process.");
    ];
    requires_scramble_game = false;
    requires_sequence_game = true; (* Trigger the sequence game *)
  }
]

(* Display a single scenario and get the user's choice *)
let rec handle_scenario scenario points used_words =
  printf "\n%s\n" scenario.description;
  List.iteri (fun i (option, _, _) -> printf "%d) %s\n" (i + 1) option) scenario.options;
  printf "Enter your choice: ";
  match read_line () with
  | exception End_of_file ->
      printf "No input received. Exiting game.\n";
      points, used_words (* Return current points and used words *)
  | input -> (
      try
        let choice = int_of_string input in
        if choice < 1 || choice > List.length scenario.options then (
          printf "Choice out of range. No points awarded.\n";
          handle_scenario scenario points used_words (* Retry current scenario *)
        ) else (
          let _, points_for_choice, explanation = List.nth scenario.options (choice - 1) in
          printf "\n%s\n" explanation;
          
          (* Handle the scramble game or sequence game, if required, but only play once *)
          let updated_points = points + points_for_choice in
          let final_points =
            if scenario.requires_scramble_game then (
              printf "\nSolve a scrambled puzzle to proceed:\n";
              Scramble.play_game 3; (* Play scramble game once *)
              updated_points
            ) else if scenario.requires_sequence_game then (
              printf "\nSolve a sequence puzzle to proceed:\n";
              Sequence.play_sequence_game (); (* Play sequence game once *)
              updated_points
            ) else
              updated_points
          in

          final_points, used_words (* Return updated points and used words *)
        )
      with
      | Failure _ ->
          printf "Invalid input. Please enter a valid number.\n";
          handle_scenario scenario points used_words (* Retry current scenario *)
      | Invalid_argument _ ->
          printf "An error occurred. Please try again.\n";
          handle_scenario scenario points used_words (* Retry current scenario *)
    )

(* Play the Traffic Cop game *)
let play_traffic_cop () =
  let rec play_scenarios scenarios points used_words =
    match scenarios with
    | [] ->
        printf "\nGame over. Your final score is: %d\n" points
    | scenario :: remaining_scenarios ->
        let updated_points, updated_used_words = handle_scenario scenario points used_words in
        if updated_points <= 0 then
          printf "\nGame over. You lost all your points. Final score: %d\n" updated_points
        else
          play_scenarios remaining_scenarios updated_points updated_used_words
  in
  printf "Welcome to the Traffic Cop Game!\n";
  play_scenarios scenarios 5 [] (* Start with 5 points and an empty used_words list *)
