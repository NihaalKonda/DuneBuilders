open Printf
open Scramble
open Sequence
open Reactiontime
open Mathgame

type scenario = {
  description : string;
  options : (string * int * string) list; (* Option text, associated points, and explanation *)
  requires_scramble_game : bool; (* Does this scenario include the scramble game? *)
  requires_sequence_game : bool; (* Does this scenario include the sequence game? *)
  requires_reaction_game : bool; (* Does this scenario include the reaction game? *)
  requires_math_quiz : bool; (* Does this scenario include the math quiz? *)
}

(* Define a list of scenarios for Traffic Cop *)
let scenarios =
  [
    {
      description =
        "You receive a call about a driver weaving erratically through traffic. You need to act quickly.";
      options =
        [
          ("Pull the driver over for questioning", 2, "This is the safest and most responsible action.");
          ("Ignore the call and continue patrolling", -1, "Neglecting this can lead to a dangerous situation.");
          ("Request additional backup before proceeding", 1, "This ensures safety but delays action.");
        ];
      requires_scramble_game = false;
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_math_quiz = false;
    };
    {
      description =
        "A car is parked illegally in a handicapped spot without a permit. While investigating, you find a clue that requires solving a scrambled word puzzle to verify parking records.";
      options =
        [
          ("Solve the puzzle to uncover more details", 2, "This ensures you investigate thoroughly and find the necessary information.");
          ("Ignore the puzzle and tow the car", -1, "This resolves the issue but might miss important information.");
          ("Call for help to solve the puzzle", 1, "This is cautious but slows down the investigation.");
        ];
      requires_scramble_game = true;
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_math_quiz = false;
    };
    {
      description =
        "While monitoring traffic, you stop a vehicle with mismatched plates. Running the plates triggers a system alert requiring you to solve a pattern sequence to unlock detailed vehicle information.";
      options =
        [
          ("Solve the sequence to proceed", 2, "This helps you investigate the potential vehicle mismatch quickly.");
          ("Let the driver go with a warning", -1, "This neglects your duty to investigate properly.");
          ("Call for backup and delay the investigation", 1, "This ensures safety but slows down the process.");
        ];
      requires_scramble_game = false;
      requires_sequence_game = true;
      requires_reaction_game = false;
      requires_math_quiz = false;
    };
    (* Add more scenarios as needed *)
  ]

let rec handle_scenario scenario points =
  printf "\n%s\n" scenario.description;
  List.iteri
    (fun i (option, _, _) -> printf "%d) %s\n" (i + 1) option)
    scenario.options;
  printf "Enter your choice: ";
  match read_line () with
  | exception End_of_file ->
      printf "No input received. Exiting game.\n";
      points
  | input -> (
      try
        let choice = int_of_string input in
        if choice < 1 || choice > List.length scenario.options then (
          printf "Choice out of range. No points awarded.\n";
          handle_scenario scenario points)
        else
          let _, points_for_choice, explanation =
            List.nth scenario.options (choice - 1)
          in
          printf "\n%s\n" explanation;

          (* Handle additional games based on scenario requirements *)
          let updated_points = points + points_for_choice in
          if scenario.requires_scramble_game then (
            printf "\nSolve a scrambled puzzle to proceed:\n";
            Scramble.play_game ();
            updated_points)
          else if scenario.requires_sequence_game then (
            printf "\nSolve a sequence puzzle to proceed:\n";
            Sequence.play_sequence_game ();
            updated_points)
          else if scenario.requires_reaction_game then (
            printf "\nTest your reflexes to proceed:\n";
            Reactiontime.play_reaction_game ();
            updated_points)
          else if scenario.requires_math_quiz then (
            printf "\nSolve a math quiz to proceed:\n";
            Mathgame.run_quiz ();
            updated_points)
          else updated_points
      with
      | Failure _ ->
          printf "Invalid input. Please enter a valid number.\n";
          handle_scenario scenario points
      | Invalid_argument _ ->
          printf "An error occurred. Please try again.\n";
          handle_scenario scenario points))

let play_traffic_cop () =
  let rec play_scenarios scenarios points =
    match scenarios with
    | [] -> printf "\nGame over. Your final score is: %d\n" points
    | scenario :: remaining_scenarios ->
        let updated_points = handle_scenario scenario points in
        if updated_points <= 0 then
          printf "\nGame over. You lost all your points. Final score: %d\n"
            updated_points
        else play_scenarios remaining_scenarios updated_points
  in
  printf "Welcome to the Traffic Cop Game!\n";
  play_scenarios scenarios 5
