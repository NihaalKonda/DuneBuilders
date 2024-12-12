open Printf
open Scramble
open Sequence
open Reactiontime
open Mathgame

type scenario = {
  description : string;
  options : (string * int * string) list;
  requires_scramble_game : bool;
  requires_sequence_game : bool;
  requires_reaction_game : bool;
  requires_math_game : bool;
}

let create_scenario desc opts scramble sequence reaction math =
  {
    description = desc;
    options = opts;
    requires_scramble_game = scramble;
    requires_sequence_game = sequence;
    requires_reaction_game = reaction;
    requires_math_game = math;
  }

let rec handle_scenario scenario points =
  printf "\n%s\n" scenario.description;
  List.iteri
    (fun i (option, _, _) -> printf "%d) %s\n" (i + 1) option)
    scenario.options;
  printf "Enter your choice: ";
  let rec get_valid_input attempts =
    if attempts > 3 then (
      printf "Too many invalid attempts. Exiting game.\n";
      None)
    else
      match read_line () with
      | exception End_of_file -> None
      | input -> (
          try
            let choice = int_of_string input in
            if choice < 1 || choice > List.length scenario.options then (
              printf "Choice out of range. Try again: ";
              get_valid_input (attempts + 1))
            else Some choice
          with Failure _ ->
            printf "Invalid input. Try again: ";
            get_valid_input (attempts + 1))
  in
  match get_valid_input 0 with
  | None -> points (* Exit if no valid input *)
  | Some choice ->
      let _, points_for_choice, explanation =
        List.nth scenario.options (choice - 1)
      in
      printf "\n%s\n" explanation;

      let updated_points = points + points_for_choice in

      (* Mini-games contribution *)
      let updated_points =
        if scenario.requires_sequence_game then (
          printf "\nTime to test your problem-solving skills!\n";
          updated_points + Sequence.play_sequence_game ())
        else updated_points
      in
      let updated_points =
        if scenario.requires_reaction_game then (
          printf "\nYou need quick reflexes! Let's test your reaction time.\n";
          updated_points + Reactiontime.play_reaction_game ())
        else updated_points
      in
      let updated_points =
        if scenario.requires_scramble_game then (
          printf "\nSolve a scrambled puzzle to proceed:\n";
          updated_points + Scramble.play_game ())
        else updated_points
      in
      let updated_points =
        if scenario.requires_math_game then (
          printf "\nYou must solve a series of math problems to proceed:\n";
          updated_points + Mathgame.play_quiz ())
        else updated_points
      in
      updated_points

let rec play_scenarios scenarios points =
  match scenarios with
  | [] ->
      printf "\nGame over. Your final score is: %d\n" points;
      points
  | scenario :: remaining_scenarios ->
      let updated_points = handle_scenario scenario points in
      if updated_points <= 0 then (
        printf "\nGame over. You lost all your points. Final score: %d\n"
          updated_points;
        updated_points)
      else play_scenarios remaining_scenarios updated_points
