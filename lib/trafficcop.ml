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
  requires_math_quiz : bool;
}

let scenarios =
  [
    {
      description =
        "A suspicious vehicle is pulled over, and the driver's story doesn't \
         add up. To verify the driver's details, you must solve a math quiz.";
      options =
        [
          ( "Solve the math quiz to proceed",
            2,
            "Solving the quiz will allow you to verify the driver's \
             information." );
          ( "Let the driver go",
            -1,
            "This could let a potential offender go free." );
          ( "Call for backup and delay verification",
            1,
            "This ensures safety but delays resolution." );
        ];
      requires_scramble_game = false;
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_math_quiz = true;
    };
    {
      description =
        "You are called to a minor fender-bender in a campus parking lot. Both \
         drivers are cooperative and no one is injured.";
      options =
        [
          ( "Help both parties exchange information and file a report",
            2,
            "This is the most professional approach." );
          ( "Ignore the situation and focus on more pressing tasks",
            -1,
            "This neglects your responsibility." );
          ( "Suggest they settle the matter privately",
            1,
            "This resolves the issue but may lack proper documentation." );
        ];
      requires_scramble_game = false;
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_math_quiz = false;
    };
    {
      description =
        "A driver suddenly speeds through a red light, and you need to respond \
         immediately.";
      options =
        [
          ( "Chase after the driver",
            2,
            "Quick reflexes are crucial to respond to such an incident." );
          ( "Report the incident and let it go",
            -1,
            "This neglects your duty to act immediately." );
          ( "Call for backup and track the driver",
            1,
            "This ensures safety but may result in delays." );
        ];
      requires_scramble_game = false;
      requires_sequence_game = false;
      requires_reaction_game = true;
      requires_math_quiz = false;
    };
    {
      description =
        "An unattended package is found in a public area. Solving a numerical \
         sequence might reveal its owner.";
      options =
        [
          ( "Solve the sequence to identify the owner",
            2,
            "This could provide valuable information about the package." );
          ( "Evacuate the area and call for backup",
            1,
            "This ensures safety but delays resolution." );
          ( "Ignore the package and continue patrolling",
            -2,
            "This is neglectful and could result in harm." );
        ];
      requires_scramble_game = false;
      requires_sequence_game = true;
      requires_reaction_game = false;
      requires_math_quiz = false;
    };
    {
      description =
        "You find a bag of evidence with an encoded message that could lead to \
         a suspect. To decode it, solve a scrambled word puzzle.";
      options =
        [
          ( "Solve the puzzle to decode the message",
            2,
            "This ensures you can use the evidence to find the suspect." );
          ( "Ignore the evidence and move on",
            -1,
            "This neglects critical information." );
          ( "Call for assistance to analyze the evidence",
            1,
            "This might work but delays the investigation." );
        ];
      requires_scramble_game = true;
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_math_quiz = false;
    };
    {
      description =
        "A pedestrian reports suspicious behavior near a campus building. You \
         investigate but find no immediate threat.";
      options =
        [
          ( "Reassure the pedestrian and file a report",
            2,
            "This calms the pedestrian and ensures accountability." );
          ( "Ignore the report and continue patrolling",
            -1,
            "This neglects your duty and may cause concern." );
          ( "Request additional patrols in the area",
            1,
            "This is a cautious approach but may not reassure the pedestrian \
             fully." );
        ];
      requires_scramble_game = false;
      requires_sequence_game = false;
      requires_reaction_game = false;
      requires_math_quiz = false;
    };
  ]

let rec handle_scenario scenario points used_words =
  printf "\n%s\n" scenario.description;
  List.iteri
    (fun i (option, _, _) -> printf "%d) %s\n" (i + 1) option)
    scenario.options;
  printf "Enter your choice: ";
  match read_line () with
  | exception End_of_file ->
      printf "No input received. Exiting game.\n";
      (points, used_words)
  | input -> (
      try
        let choice = int_of_string input in
        if choice < 1 || choice > List.length scenario.options then (
          printf "Choice out of range. No points awarded.\n";
          handle_scenario scenario points used_words)
        else
          let _, points_for_choice, explanation =
            List.nth scenario.options (choice - 1)
          in
          printf "\n%s\n" explanation;
          let updated_points = points + points_for_choice in
          let final_points =
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
          in
          (final_points, used_words)
      with
      | Failure _ ->
          printf "Invalid input. Please enter a valid number.\n";
          handle_scenario scenario points used_words
      | Invalid_argument _ ->
          printf "An error occurred. Please try again.\n";
          handle_scenario scenario points used_words)

let play_traffic_cop () =
  let rec play_scenarios scenarios points used_words =
    match scenarios with
    | [] -> printf "\nGame over. Your final score is: %d\n" points
    | scenario :: remaining_scenarios ->
        let updated_points, updated_used_words =
          handle_scenario scenario points used_words
        in
        if updated_points <= 0 then
          printf "\nGame over. You lost all your points. Final score: %d\n"
            updated_points
        else
          play_scenarios remaining_scenarios updated_points updated_used_words
  in
  printf "Welcome to the Traffic Cop Game!\n";
  play_scenarios scenarios 5 []
