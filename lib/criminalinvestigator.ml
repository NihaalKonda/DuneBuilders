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

let scenarios =
  [
    {
      description =
        "You arrive at the crime scene and notice several potential leads."
        ^ "What do you do first?";
      options =
        [
          ( "Examine the body in detail",
            2,
            "You carefully examine the body for clues, uncovering vital \
             evidence." );
          ( "Question witnesses",
            1,
            "Talking to witnesses provides context but may lack crucial \
             details." );
          ( "Secure the perimeter and search for clues",
            2,
            "A thorough search reveals critical evidence that was initially \
             overlooked." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_scramble_game = true;
      requires_math_game = false;
    };
    {
      description =
        "A suspect is fleeing the crime scene. What is your immediate action?";
      options =
        [
          ( "Chase them on foot",
            2,
            "Your quick reflexes allow you to catch the suspect before they \
             escape." );
          ( "Call for backup",
            1,
            "This ensures safety but might give the suspect time to escape." );
          ( "Try to cut them off using your car",
            1,
            "This strategy might work but requires precise coordination." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = true;
      requires_scramble_game = false;
      requires_math_game = false;
    };
    {
      description =
        "A key witness is nervous and unwilling to talk or provide any further \
         information. What do you do?";
      options =
        [
          ( "Offer them protection",
            2,
            "This is the right thing to do because it abides with the law." );
          ( "Persuade them with evidence",
            1,
            "Persuading the witness is not a good idea, they should speak if \
             they want." );
          ( "Threaten to arrest them",
            -1,
            "It is illegal to threaten a witness in a crime scene because they \
             have rights allowing them to only speak if they want." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_scramble_game = false;
      requires_math_game = false;
    };
    {
      description =
        "You find a coded message at the scene. Deciphering it might reveal \
         vital information.";
      options =
        [
          ( "Solve the code yourself",
            2,
            "Your analytical skills uncover critical details hidden in the \
             code." );
          ( "Send it to the lab for analysis",
            1,
            "This may yield results but takes more time." );
          ( "Ignore the code and focus on other leads",
            -1,
            "You risk missing an essential piece of evidence." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_scramble_game = true;
      requires_math_game = false;
    };
    {
      description =
        "A financial record linked to the case appears encrypted. Solving the \
         puzzle could be key.";
      options =
        [
          ( "Work through the math problems to decode it",
            2,
            "Your effort reveals hidden details that advance the investigation."
          );
          ( "Delegate the task to a specialist",
            1,
            "This ensures accuracy but takes longer." );
          ( "Skip the encryption and move to other tasks",
            -1,
            "You risk overlooking crucial evidence." );
        ];
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_scramble_game = false;
      requires_math_game = true;
    };
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
          let updated_points = points + points_for_choice in

          if scenario.requires_sequence_game then (
            printf
              "\n\
               Time to test your problem-solving skills with the sequence game!\n";
            Sequence.play_sequence_game ());

          if scenario.requires_reaction_game then (
            printf "\nYou need quick reflexes! Let's test your reaction time.\n";
            Reactiontime.play_reaction_game ());

          if scenario.requires_scramble_game then (
            printf "\nSolve a scrambled puzzle to proceed:\n";
            Scramble.play_game ());

          if scenario.requires_math_game then (
            printf "\nYou must solve a series of math problems to proceed:\n";
            Mathgame.run_quiz ());

          updated_points
      with
      | Failure _ ->
          printf "Invalid input. Please enter a valid number.\n";
          handle_scenario scenario points
      | Invalid_argument _ ->
          printf "An error occurred. Please try again.\n";
          handle_scenario scenario points)

let play_criminal_investigator () =
  let rec play_scenarios scenarios points =
    match scenarios with
    | [] ->
        printf "\nGame over. Your final score is: %d\n" points;
        points (* Return the final score when scenarios are exhausted *)
    | scenario :: remaining_scenarios ->
        let updated_points = handle_scenario scenario points in
        if updated_points <= 0 then (
          printf "\nGame over. You lost all your points. Final score: %d\n"
            updated_points;
          updated_points
          (* Return the final score when points drop to 0 or below *))
        else play_scenarios remaining_scenarios updated_points
  in
  printf "Welcome to the Criminal Investigator Simulation!\n";
  play_scenarios scenarios 5 (* Start the game with 5 points *)
