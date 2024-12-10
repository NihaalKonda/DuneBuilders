open Printf
open Sequence
open Reactiontime
open Scramble
open Mathgame

(* Extend the scenario type to include puzzle requirements *)
type scenario = {
  description : string;
  options : (string * int * string) list;
      (* Option text, points, and explanation *)
  requires_sequence_game : bool;
      (* Does this scenario include the sequence game? *)
  requires_reaction_game : bool;
      (* Does this scenario include the reaction time game? *)
  requires_scramble_game : bool;
      (* Does this scenario include the scramble game? *)
  requires_math_game : bool; (* Does this scenario include the math game? *)
}

(* Define a list of scenarios for a Border Patrol Agent *)
let scenarios =
  [
    {
      description =
        "You spot an individual lingering near the border fence after dusk.\n\
         They appear nervous and are avoiding well-lit areas.";
      options =
        [
          ( "Approach the individual and question them",
            2,
            "By calmly approaching and inquiring, you gather information and \
             possibly prevent illegal entry without escalating the situation."
          );
          ( "Call for immediate backup and surround the area",
            1,
            "This might ensure safety but could cause unnecessary alarm if the \
             individual is not a threat." );
          ( "Ignore the individual and move on to another patrol point",
            -1,
            "This may allow potential illegal activity to go unchecked." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_scramble_game = false;
      requires_math_game = false;
    };
    {
      description =
        "A suspicious vehicle approaches the checkpoint.\n\
         You notice the driver is acting unusually nervous and the passenger \
         looks tense.";
      options =
        [
          ( "Ask the driver to step out and conduct a thorough search",
            2,
            "A direct, legal search could reveal contraband and ensure the \
             integrity of the checkpoint." );
          ( "Let the vehicle pass after a quick ID check",
            -2,
            "This risks missing potential contraband or illegal items." );
          ( "Request additional identification and verify vehicle \
             documentation thoroughly",
            1,
            "This is a safer approach than waving them through, but not as \
             proactive as searching immediately." );
        ];
      requires_sequence_game = true;
      (* Trigger the sequence game *)
      requires_reaction_game = false;
      requires_scramble_game = false;
      requires_math_game = false;
    };
    {
      description =
        "You receive a tip about a hidden compartment in a cargo truck \
         carrying fresh produce.\n\
         The driver has proper documentation but seems evasive when questioned \
         about the cargo.";
      options =
        [
          ( "Perform a detailed inspection of the cargo and vehicle",
            2,
            "Thoroughly checking the cargo can prevent illegal items from \
             passing through undetected." );
          ( "Take the driver's word and expedite the truck to keep traffic \
             flowing",
            -1,
            "This could allow contraband to enter, compromising border \
             security." );
          ( "Call for a canine unit to aid in the inspection",
            1,
            "A canine unit can assist without causing unnecessary delay if the \
             driver is compliant." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_scramble_game = false;
      requires_math_game = false;
    };
    {
      description =
        "As you talk to a person at the checkpoint, they suddenly attempt to \
         run.\n\
         You must react quickly to subdue them safely.";
      options =
        [
          ( "Attempt to grab them immediately",
            2,
            "A quick, decisive action might prevent escape, but you must be \
             quick and accurate." );
          ( "Call for help first",
            1,
            "This might be safer, but could give them time to flee." );
          ( "Do nothing and observe their movements",
            -1,
            "This gives the suspect time to escape and is not ideal." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = true;
      (* Trigger the reaction time game *)
      requires_scramble_game = false;
      requires_math_game = false;
    };
    {
      description =
        "A car is parked illegally in a restricted area near the border.\n\
         While investigating, you discover a hidden document that appears \
         scrambled. Solving this puzzle may reveal crucial information.";
      options =
        [
          ( "Solve the puzzle to uncover more details",
            2,
            "By solving the scrambled puzzle, you ensure you don't miss \
             important clues." );
          ( "Ignore the puzzle and have the car towed",
            -1,
            "This resolves the immediate issue but you might miss critical \
             information." );
          ( "Call for help to solve the puzzle",
            1,
            "Cautious but slower, you might still learn something useful." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_scramble_game = true;
      (* Trigger the scramble game *)
      requires_math_game = false;
    };
    {
      description =
        "You're reviewing suspicious financial records tied to a smuggling ring.\n\
         To access the encrypted files, you must solve a series of math \
         problems to unlock the data.";
      options =
        [
          ( "Solve the math puzzles to gain access",
            2,
            "Demonstrating your analytical skills can help you uncover hidden \
             operations." );
          ( "Ignore the math puzzles and move on",
            -1,
            "You'll miss potentially critical intelligence." );
          ( "Call for a specialist in financial crimes",
            1,
            "Might help, but delays the investigation." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_scramble_game = false;
      requires_math_game = true;
      (* Trigger the math game *)
    };
  ]

(* Display a single scenario and get the user's choice *)
let rec handle_scenario scenario points =
  printf "\n%s\n" scenario.description;
  List.iteri
    (fun i (option, _, _) -> printf "%d) %s\n" (i + 1) option)
    scenario.options;
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
          handle_scenario scenario points (* Retry current scenario *))
        else
          let _, points_for_choice, explanation =
            List.nth scenario.options (choice - 1)
          in
          printf "\n%s\n" explanation;
          let updated_points = points + points_for_choice in

          (* Handle the sequence game, if required *)
          if scenario.requires_sequence_game then (
            printf
              "\n\
               Time to test your problem-solving skills with the sequence game!\n";
            Sequence.play_sequence_game ());

          (* Handle the reaction time game, if required *)
          if scenario.requires_reaction_game then (
            printf "\nYou need quick reflexes! Let's test your reaction time.\n";
            Reactiontime.play_reaction_game ());

          (* Handle the scramble game, if required *)
          if scenario.requires_scramble_game then (
            printf "\nSolve a scrambled puzzle to proceed:\n";
            Scramble.play_game 3 (* Play scramble game with 3 rounds *));

          (* Handle the math game, if required *)
          if scenario.requires_math_game then (
            printf "\nYou must solve a series of math problems to proceed:\n";
            Mathgame.run_quiz ()
            (* Solve 5 math questions in a row as per mathgame.ml *));

          updated_points
      with
      | Failure _ ->
          printf "Invalid input. Please enter a valid number.\n";
          handle_scenario scenario points (* Retry current scenario *)
      | Invalid_argument _ ->
          printf "An error occurred. Please try again.\n";
          handle_scenario scenario points (* Retry current scenario *))

(* Play the Border Patrol Agent game *)
let play_border_patrol () =
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
  printf "Welcome to the Border Patrol Training Simulation!\n";
  play_scenarios scenarios 5 (* Start with 5 points *)
