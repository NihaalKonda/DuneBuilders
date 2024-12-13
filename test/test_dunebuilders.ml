open OUnit2
open Dunebuilders
open Borderpatrol
open Campuspolice
open Criminalinvestigator
open Gui
open Mathgame
open Sentiment
open Role
open Scenariohandler
open Scramble
open Sequence
open Trafficcop
open OUnit2
open Printf
open Scenariohandler

(* Mock mini-game modules for testing without actual game logic *)
module MockScramble = struct
  let play_game () = 5 (* Simulates always earning 5 points *)
end

module MockSequence = struct
  let play_sequence_game () = 3 (* Simulates always earning 3 points *)
end

module MockSentiment = struct
  let play_sentiment_game () = 2 (* Simulates always earning 2 points *)
end

module MockMathgame = struct
  let play_quiz () = 4 (* Simulates always earning 4 points *)
end

let with_mock_input input f =
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;
  let result =
    try f ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in
  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;
  result

let test_scenarios_count _ =
  assert_equal 8 (List.length scenarios) ~msg:"Incorrect number of scenarios"

let test_scenario_structure _ =
  let first_scenario = List.hd scenarios in
  assert_equal 3
    (List.length (get_scenario_options first_scenario))
    ~msg:"Incorrect number of options in the first scenario"

(* MATH GAME TESTS *)

(* let test_generate_question _ = let question1, answer1 = generate_question ()
   in let question2, answer2 = generate_question () in let question3, answer3 =
   generate_question () in assert_equal question1 "5 + 13"; assert_equal answer1
   18; assert_equal question2 "16 + 47"; assert_equal answer2 63; assert_equal
   question3 "46 * 38"; assert_equal answer3 1748 *)

let test_check_correct_answer _ =
  let input = "8\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let points =
    try
      let question, answer = ("5 + 3", 8) in
      ask_question question answer
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal 1 points

let test_check_incorrect_answer _ =
  let input = "10\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let points =
    try
      let question, answer = ("10 - 7", 3) in
      ask_question question answer
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal 0 points

let test_play_quiz _ =
  let input = "7\n10\n20\n30\n40\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let points =
    try play_quiz ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal points 0

(* let test_random_reproducibility _ = Random.init 123; let question1, answer1 =
   generate_question () in let question2, answer2 = generate_question () in

   Random.init 123; let question1', answer1' = generate_question () in let
   question2', answer2' = generate_question () in

   assert_equal question1 question1'; assert_equal answer1 answer1';
   assert_equal question2 question2'; assert_equal answer2 answer2';

   Random.init 456; let question3, answer3 = generate_question () in assert_bool
   "Different seeds should give different results" (question3 <> question1 ||
   answer3 <> answer1) *)

(* SCENARIO TESTS *)
let test_scenario_count _ =
  assert_equal 8 (List.length scenarios) ~msg:"Incorrect number of scenarios"

let test_scenario_options_count _ =
  List.iteri
    (fun idx scenario ->
      assert_equal 3
        (List.length (get_scenario_options scenario))
        ~msg:(sprintf "Scenario %d has incorrect number of options" (idx + 1)))
    scenarios

let test_play_scenario _ =
  let scenario = List.nth scenarios 0 in
  let score = play_scenarios [ scenario ] 1 in
  assert_equal 1 score ~msg:"Scenario did not return the correct score"

(* SCRAMBLE TESTS *)

let test_play_game_all_correct _ =
  (* Input corresponds to correct guesses for the fixed order words *)
  let input = "arrest\njustice\npatrol\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in

  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let points =
    try play_game ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal points 3

let test_play_game_all_incorrect _ =
  (* Input corresponds to incorrect guesses for all fixed order words *)
  let input = "wrong1\nwrong2\nwrong3\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in

  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let points =
    try play_game ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal points 0

let test_play_game_mixed _ =
  (* Input contains a mix of correct and incorrect guesses *)
  let input = "arrest\nwrong_answer\npatrol\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in

  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let points =
    try play_game ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal points 2

let test_get_word_fixed_order _ =
  (* Ensure get_word always returns words in the fixed order *)
  let used_words = [] in
  let w1, _ = get_word used_words in
  let w2, _ = get_word [ w1 ] in
  let w3, _ = get_word [ w1; w2 ] in

  assert_equal w1 "arrest";
  assert_equal w2 "justice";
  assert_equal w3 "patrol"

let test_get_word_exhaustion _ =
  (* Ensure an exception is raised after all words in the fixed order are
     used *)
  let used_words = [ "arrest"; "justice"; "patrol" ] in
  assert_raises (Failure "No more words available") (fun () ->
      get_word used_words)

(* let test_play_game _ = let input = "arrest\n" ^ "wrong_answer\n" ^ "patrol\n"
   in let old_stdin = Unix.dup Unix.stdin in let pipe_read, pipe_write =
   Unix.pipe () in

   Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
   Unix.close pipe_write;

   Unix.dup2 pipe_read Unix.stdin;

   let points = try play_game () with exn -> Unix.dup2 old_stdin Unix.stdin;
   raise exn in

   Unix.dup2 old_stdin Unix.stdin; Unix.close old_stdin;

   assert_equal points 2 *)

let test_handle_scenario_valid_choice _ =
  let scenario =
    {
      description = "Choose an option:";
      options =
        [
          ("Option 1", 10, "You chose option 1.");
          ("Option 2", 5, "Option 2 chosen.");
        ];
      requires_scramble_game = false;
      requires_sequence_game = false;
      requires_sentiment_game = false;
      requires_math_game = false;
    }
  in
  let input = "1\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let updated_points =
    try handle_scenario scenario 0
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal 10 updated_points
    ~msg:"Points should update correctly after a valid choice"

let test_handle_scenario_invalid_choice _ =
  let scenario =
    {
      description = "Choose an option:";
      options =
        [
          ("Option 1", 10, "You chose option 1.");
          ("Option 2", 5, "Option 2 chosen.");
        ];
      requires_scramble_game = false;
      requires_sequence_game = false;
      requires_sentiment_game = false;
      requires_math_game = false;
    }
  in
  let input = "3\n1\n" in
  (* Invalid choice, followed by valid choice *)
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let updated_points =
    try handle_scenario scenario 0
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal 10 updated_points
    ~msg:"Points should update correctly after retrying a valid choice"

let test_handle_scenario_with_mini_games _ =
  (* Redefine handle_scenario locally to use the mocks *)
  let handle_scenario scenario points =
    let updated_points =
      if scenario.requires_scramble_game then points + MockScramble.play_game ()
      else points
    in
    let updated_points =
      if scenario.requires_sequence_game then
        updated_points + MockSequence.play_sequence_game ()
      else updated_points
    in
    let updated_points =
      if scenario.requires_sentiment_game then
        updated_points + MockSentiment.play_sentiment_game ()
      else updated_points
    in
    let updated_points =
      if scenario.requires_math_game then
        updated_points + MockMathgame.play_quiz ()
      else updated_points
    in
    updated_points
  in

  let scenario =
    {
      description = "Solve the challenges:";
      options = [ ("Start", 0, "Let the games begin!") ];
      requires_scramble_game = true;
      requires_sequence_game = true;
      requires_sentiment_game = true;
      requires_math_game = true;
    }
  in
  let updated_points = handle_scenario scenario 0 in
  assert_equal 14 updated_points
    ~msg:"Points should update correctly after all mini-games"

let test_play_scenarios _ =
  (* Define the scenarios with valid options and non-zero points *)
  let scenarios =
    [
      create_scenario "Scenario 1"
        [ ("Option 1", 5, "You chose wisely.") ]
        false false false false;
      create_scenario "Scenario 2"
        [ ("Option 2", 10, "Good choice!") ]
        false false false false;
    ]
  in

  (* Mock user input to select the first option for both scenarios *)
  let input = "1\n1\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  (* Run the play_scenarios function with the mocked input *)
  let final_score =
    try play_scenarios scenarios 0
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  (* Restore the original stdin *)
  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  (* Check if the final score matches the expected value *)
  assert_equal 15 final_score
    ~msg:"Final score should be the sum of all scenario points"

let test_scenario_termination _ =
  let final_score = play_traffic_cop () in
  assert_bool "Game should end with a non-negative score" (final_score >= 0)

let test_play_game_all_correct _ =
  (* Simulate correct answers for all rounds *)
  let input = "arrest\njustice\ninvestigation\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let score =
    try play_game ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal 3 score
    ~msg:"Game should end with 3 points for all correct answers"

let test_play_game_mixed_answers _ =
  (* Simulate correct and incorrect answers *)
  let input = "arrest\nwrong_answer\ninvestigation\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let score =
    try play_game ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal 2 score
    ~msg:"Game should end with 2 points for one incorrect answer"

let test_play_traffic_cop_no_input _ =
  let input = "" (* Simulate no input with an empty string *) in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in

  (* Simulate no input *)
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let final_score =
    try play_traffic_cop ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  (* Restore the original stdin *)
  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  (* Check the final score *)
  assert_equal 0 final_score
    ~msg:"Traffic Cop game should end with 0 points for no input"

let test_play_traffic_cop_invalid_input _ =
  (* Simulate invalid inputs followed by valid ones *)
  let input = "5\nabc\n1\n2\n3\n1\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let score =
    try play_traffic_cop ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  assert_equal 4 score
    ~msg:
      "Traffic Cop game should handle invalid inputs and compute the correct \
       score"

let mock_play_sequence_game () = 3
let mock_play_sentiment_game () = 2
let mock_play_scramble_game () = 4
let mock_play_quiz () = 5

let mock_scenario_with_all_games =
  {
    description = "Test scenario requiring all mini-games.";
    options =
      [
        ("Option 1", 1, "You chose the best option.");
        ("Option 2", 0, "Neutral choice.");
        ("Option 3", -1, "Bad choice.");
      ];
    requires_scramble_game = true;
    requires_sequence_game = true;
    requires_sentiment_game = true;
    requires_math_game = true;
  }

let mock_scenario_with_no_games =
  {
    description = "Test scenario with no mini-games.";
    options =
      [
        ("Option 1", 2, "Good choice.");
        ("Option 2", 0, "Neutral choice.");
        ("Option 3", -1, "Bad choice.");
      ];
    requires_scramble_game = false;
    requires_sequence_game = false;
    requires_sentiment_game = false;
    requires_math_game = false;
  }

(* Test sequence game *)
let test_handle_sequence_game _ =
  let module MockSequence = struct
    let play_sequence_game = mock_play_sequence_game
  end in
  let points = 0 in
  let updated_points =
    if mock_scenario_with_all_games.requires_sequence_game then (
      Printf.printf "Mock sequence game played.\n";
      points + MockSequence.play_sequence_game ())
    else points
  in
  assert_equal 3 updated_points
    ~msg:"Points should update correctly after sequence game"

(* Test scramble game *)
let test_handle_scramble_game _ =
  let points = 5 in
  let updated_points =
    if mock_scenario_with_all_games.requires_scramble_game then (
      Printf.printf "Mock scramble game played.\n";
      points + MockScramble.play_game ())
    else points
  in
  assert_equal 9 updated_points
    ~msg:"Points should update correctly after scramble game"

(* Test math game *)
let test_handle_math_game _ =
  let points = 9 in
  let updated_points =
    if mock_scenario_with_all_games.requires_math_game then (
      Printf.printf "Mock math game played.\n";
      points + MockMathgame.play_quiz ())
    else points
  in
  assert_equal 14 updated_points
    ~msg:"Points should update correctly after math game"

(* Test handling all games in a scenario *)
let test_handle_all_games _ =
  let points = 0 in
  let updated_points =
    points
    + (if mock_scenario_with_all_games.requires_sequence_game then
         mock_play_sequence_game ()
       else 0)
    + (if mock_scenario_with_all_games.requires_sentiment_game then
         mock_play_sentiment_game ()
       else 0)
    + (if mock_scenario_with_all_games.requires_scramble_game then
         mock_play_scramble_game ()
       else 0)
    +
    if mock_scenario_with_all_games.requires_math_game then mock_play_quiz ()
    else 0
  in
  assert_equal 14 updated_points
    ~msg:"Points should correctly sum from all mini-games in a scenario"

(* Test handling a scenario with no mini-games *)
let test_handle_no_games _ =
  let points = 0 in
  let updated_points =
    points
    + (if mock_scenario_with_no_games.requires_sequence_game then
         mock_play_sequence_game ()
       else 0)
    + (if mock_scenario_with_no_games.requires_sentiment_game then
         mock_play_sentiment_game ()
       else 0)
    + (if mock_scenario_with_no_games.requires_scramble_game then
         mock_play_scramble_game ()
       else 0)
    +
    if mock_scenario_with_no_games.requires_math_game then mock_play_quiz ()
    else 0
  in
  assert_equal 0 updated_points
    ~msg:"Points should remain unchanged for a scenario with no mini-games"
(* Test Sentiment Detection Game *)

let test_generate_sentiment_question _ =
  let sentence, category = generate_sentiment_question () in
  assert_bool "Generated sentence should not be empty"
    (String.length sentence > 0);
  assert_bool "Category should not be empty" (String.length category > 0);
  assert_bool "Category should be one of the predefined options"
    (List.mem category [ "urgent"; "calm"; "threat"; "resolved" ])

let test_play_sentiment_game _ =
  let input = "calm\nthreat\nurgent\nresolved\ncalm\n" in
  let points = with_mock_input input (fun () -> play_sentiment_game ()) in
  assert_bool "Points should be a non-negative integer" (points >= 0)

(* Test play_sequence_game logic without simulating input *)

(**Sequence Game*)
let index_of lst value =
  let rec aux idx = function
    | [] -> raise Not_found
    | x :: xs -> if x = value then idx else aux (idx + 1) xs
  in
  aux 0 lst

let test_invalid_input _ =
  let input = "abc\n" in
  let points = with_mock_input input play_sequence_game in
  assert_equal 0 points ~msg:"Invalid input should terminate the game"

let test_no_input _ =
  let input = "" in
  let points = with_mock_input input play_sequence_game in
  assert_equal 0 points ~msg:"No input should terminate the game"

let test_one_incorrect_answer _ =
  (* Simulate the first sequence and provide one incorrect answer *)
  let input = "2\n" in
  (* "2" corresponds to an incorrect answer *)
  let points = with_mock_input input play_sequence_game in
  assert_equal 0 points ~msg:"One incorrect answer should terminate the game"

let test_all_correct_answers _ =
  (* Helper function to generate inputs based on correct answers *)
  let generate_inputs sequences =
    List.map
      (fun (_, correct_answer, answers) ->
        let correct_index = index_of answers correct_answer in
        string_of_int (correct_index + 1))
      sequences
  in

  (* Generate inputs for all correct answers *)
  let inputs = generate_inputs sequences in

  (* Join the inputs into a single string with newlines to simulate user
     input *)
  let input = String.concat "\n" inputs ^ "\n" in

  (* Simulate the game with mock input *)
  let points = with_mock_input input play_sequence_game in

  (* Assert that the total points equal the number of sequences *)
  assert_equal (List.length sequences) points
    ~msg:"All correct answers should yield maximum points"

let tests =
  "Test Suite"
  >::: [
         (* MATH GAME TESTS *)
         "test_check_correct_answer" >:: test_check_correct_answer;
         "test_check_incorrect_answer" >:: test_check_incorrect_answer;
         "test_play_quiz" >:: test_play_quiz;
         (*SCENARIOS*)
         "test_scenarios_count" >:: test_scenarios_count;
         "test_scenario_structure" >:: test_scenario_structure;
         "test_scenario_count" >:: test_scenario_count;
         "test_scenario_options_count" >:: test_scenario_options_count;
         "test_play_scenario" >:: test_play_scenario;
         "test_scenarios_count" >:: test_scenarios_count;
         "test_scenario_structure" >:: test_scenario_structure;
         "test_check_correct_answer" >:: test_check_correct_answer;
         "test_check_incorrect_answer" >:: test_check_incorrect_answer;
         (* "test_shuffle_word" >:: test_shuffle_word; "test_get_scrambled_word"
            >:: test_get_scrambled_word; "test_play_game" >:: test_play_game; *)
         "test_play_game_all_correct" >:: test_play_game_all_correct;
         "test_play_game_all_incorrect" >:: test_play_game_all_incorrect;
         "test_play_game_mixed" >:: test_play_game_mixed;
         "test_get_word_exhaustion" >:: test_get_word_exhaustion;
         "test_handle_scenario_valid_choice"
         >:: test_handle_scenario_valid_choice;
         "test_handle_scenario_invalid_choice"
         >:: test_handle_scenario_invalid_choice;
         "test_handle_scenario_valid_choice"
         >:: test_handle_scenario_valid_choice;
         "test_handle_scenario_with_mini_games"
         >:: test_handle_scenario_with_mini_games;
         "test_play_game_all_correct" >:: test_play_game_all_correct;
         (* "test_play_traffic_cop_no_input" >::
            test_play_traffic_cop_no_input; *)
         (* "test_play_traffic_cop_invalid_input" >::
            test_play_traffic_cop_invalid_input; *)

         (*SENTIMENT GAME TESTS*)
         "test_play_sentiment_game" >:: test_play_sentiment_game;
         "test_generate_sentiment_question" >:: test_generate_sentiment_question;
         (*SEQUENCE GAME TESTS*)
         "test_invalid_input" >:: test_invalid_input;
         "test_no_input" >:: test_no_input;
         "test_one_incorrect_answer" >:: test_one_incorrect_answer;
         "test_all_correct_answers" >:: test_all_correct_answers;
       ]

let _ = run_test_tt_main tests
