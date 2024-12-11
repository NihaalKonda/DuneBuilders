open Printf
open Scramble
open Sequence
open Reactiontime
open Mathgame

(* Define the structure for a scenario *)
type scenario = {
  description : string;
  choices : (string * int * string) list;
      (* Choice text, associated points, and explanation *)
  games_required : bool * bool * bool * bool;
      (* scramble, sequence, reaction, math *)
}

(* List of scenarios for the Campus Police Game *)
let scenario_list =
  [
    {
      description =
        "You receive a call about excessive noise in a dorm. What will you do?";
      choices =
        [
          ( "Knock and speak with the residents",
            2,
            "Professional and effective response." );
          ("Call for backup", 1, "Safe, but might escalate unnecessarily.");
          ("Issue citations immediately", -1, "Aggressive and confrontational.");
        ];
      games_required = (false, false, false, false);
    };
    {
      description =
        "You find an unmarked package in a dormitory. It might be dangerous.";
      choices =
        [
          ( "Call the bomb squad",
            2,
            "Ensures safety with professional intervention." );
          ( "Evacuate and secure the area",
            1,
            "Safety first, but delays expert assistance." );
          ( "Ignore it and continue patrolling",
            -2,
            "Reckless and risky decision." );
        ];
      games_required = (false, true, false, false);
    };
    {
      description =
        "You witness a heated argument near the campus cafeteria escalating \
         into a fight.";
      choices =
        [
          ("Intervene immediately", 2, "De-escalates the situation effectively.");
          ("Call for backup", 1, "Safe, but delays resolution.");
          ("Walk away", -1, "Neglectful and irresponsible.");
        ];
      games_required = (false, false, true, false);
    };
  ]

(* Process a single scenario *)
let rec process_scenario sc current_points =
  printf "\n%s\n" sc.description;
  List.iteri
    (fun idx (text, _, _) -> printf "%d) %s\n" (idx + 1) text)
    sc.choices;
  printf "Enter your choice: ";

  match read_line () with
  | exception End_of_file ->
      printf "No input received. Exiting game.\n";
      current_points
  | input -> (
      try
        let choice = int_of_string input in
        if choice < 1 || choice > List.length sc.choices then (
          printf "Invalid choice. Try again.\n";
          process_scenario sc current_points)
        else
          let _, points_earned, explanation =
            List.nth sc.choices (choice - 1)
          in
          printf "\n%s\n" explanation;
          let new_points = current_points + points_earned in

          (* Handle game triggers *)
          let play_scramble, play_sequence, play_reaction, play_math =
            sc.games_required
          in
          if play_scramble then (
            printf "\nScramble game activated!\n";
            Scramble.play_game ());
          if play_sequence then (
            printf "\nSequence game activated!\n";
            Sequence.play_sequence_game ());
          if play_reaction then (
            printf "\nReaction time game activated!\n";
            Reactiontime.play_reaction_game ());
          if play_math then (
            printf "\nMath quiz activated!\n";
            Mathgame.run_quiz ());

          new_points
      with Failure _ ->
        printf "Please enter a valid number.\n";
        process_scenario sc current_points)

(* Play the Campus Police game *)
let play_campus_police () =
  let rec play_game scenarios points =
    match scenarios with
    | [] ->
        printf "\nAll scenarios completed. Final score: %d\n" points;
        points
    | sc :: remaining ->
        let updated_points = process_scenario sc points in
        if updated_points <= 0 then (
          printf "\nGame over. You lost all your points.\n";
          updated_points)
        else play_game remaining updated_points
  in
  printf "Welcome to the Campus Police Game!\n";
  play_game scenario_list 5
