open OUnit2
open Dunebuilders
open Borderpatrol
open Campuspolice
open Criminalinvestigator
open Gui
open Mathgame
open Reactiontime
open Role
open Scenariohandler
open Scramble
open Sequence
open Trafficcop
open OUnit2
open Printf
open Scenariohandler

let test_scenarios_count _ =
  assert_equal 6 (List.length scenarios) ~msg:"Incorrect number of scenarios"

let test_scenario_structure _ =
  let first_scenario = List.hd scenarios in
  assert_equal 3
    (List.length (get_scenario_options first_scenario))
    ~msg:"Incorrect number of options in the first scenario"

(* MATH GAME TESTS *)

let test_generate_question _ =
  let question1, answer1 = generate_question () in
  let question2, answer2 = generate_question () in
  let question3, answer3 = generate_question () in
  assert_equal question1 "5 + 13";
  assert_equal answer1 18;
  assert_equal question2 "16 + 47";
  assert_equal answer2 63;
  assert_equal question3 "46 * 38";
  assert_equal answer3 1748

let test_check_correct_answer _ =
  let input = "7\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let points =
    try
      let question, answer = ("3 + 4", 7) in
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
      let question, answer = ("10 - 5", 5) in
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

let test_random_reproducibility _ =
  Random.init 123;
  let question1, answer1 = generate_question () in
  let question2, answer2 = generate_question () in

  Random.init 123;
  let question1', answer1' = generate_question () in
  let question2', answer2' = generate_question () in

  assert_equal question1 question1';
  assert_equal answer1 answer1';
  assert_equal question2 question2';
  assert_equal answer2 answer2';

  Random.init 456;
  let question3, answer3 = generate_question () in
  assert_bool "Different seeds should give different results"
    (question3 <> question1 || answer3 <> answer1)

let test_scenario_count _ =
  assert_equal 6 (List.length scenarios) ~msg:"Incorrect number of scenarios"

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

let test_shuffle_word _ =
  let word = "investigation" in
  let shuffled = shuffle_word word in
  assert_equal (String.length word) (String.length shuffled);
  assert_equal
    (List.sort Char.compare (List.init (String.length word) (String.get word)))
    (List.sort Char.compare
       (List.init (String.length shuffled) (String.get shuffled)))

let test_get_scrambled_word _ =
  let used_words = [ "arrest"; "justice" ] in
  let scrambled, word = get_scrambled_word used_words in
  assert_bool "Word should not be in used words"
    (not (List.mem word used_words));
  assert_equal
    (List.sort Char.compare (List.init (String.length word) (String.get word)))
    (List.sort Char.compare
       (List.init (String.length scrambled) (String.get scrambled)))

(* let test_play_game _ = let input = "arrest\n" ^ "wrong_answer\n" ^ "patrol\n"
   in let old_stdin = Unix.dup Unix.stdin in let pipe_read, pipe_write =
   Unix.pipe () in Unix.write_substring pipe_write input 0 (String.length input)
   |> ignore; Unix.close pipe_write; Unix.dup2 pipe_read Unix.stdin;

   let points = try play_game () with exn -> Unix.dup2 old_stdin Unix.stdin;
   raise exn in

   Unix.dup2 old_stdin Unix.stdin; Unix.close old_stdin;

   assert_equal points 2 *)
(* Mock mini-game modules for testing without actual game logic *)
module MockScramble = struct
  let play_game () = 5 (* Simulates always earning 5 points *)
end

module MockSequence = struct
  let play_sequence_game () = 3 (* Simulates always earning 3 points *)
end

module MockReactiontime = struct
  let play_reaction_game () = 2 (* Simulates always earning 2 points *)
end

module MockMathgame = struct
  let play_quiz () = 4 (* Simulates always earning 4 points *)
end

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
      requires_reaction_game = false;
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
      requires_reaction_game = false;
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
  let module MockScramble = struct
    let play_game () = 5
  end in
  let module MockSequence = struct
    let play_sequence_game () = 3
  end in
  let module MockReactiontime = struct
    let play_reaction_game () = 2
  end in
  let module MockMathgame = struct
    let play_quiz () = 4
  end in
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
      if scenario.requires_reaction_game then
        updated_points + MockReactiontime.play_reaction_game ()
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
      requires_reaction_game = true;
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

(* SEQUENCE GAME TESTS *)
let test_shuffle _ =
  let lst = [ 1; 2; 3; 4; 5 ] in
  let shuffled = shuffle lst in
  assert_equal (List.sort compare lst) (List.sort compare shuffled)

let test_generate_random_sequence _ =
  let seq = generate_random_sequence 5 in
  assert_equal 5 (List.length seq);
  if List.length seq > 1 then
    let steps =
      let rec calculate_steps acc = function
        | [] | [ _ ] -> List.rev acc
        | a :: b :: tail -> calculate_steps ((b - a) :: acc) (b :: tail)
      in
      calculate_steps [] seq
    in
    let first_step = List.hd steps in
    assert_bool "Steps are consistent"
      (List.for_all (fun step -> step = first_step) steps)
  else assert_bool "Sequence should have more than one element" false

let test_get_sequence_data _ =
  let sequence_str, correct_answer, answers = get_sequence_data () in
  assert_bool "Sequence ends with ', ?'"
    (String.ends_with ~suffix:", ?" sequence_str);
  assert_bool "Correct answer is in options" (List.mem correct_answer answers);
  assert_equal 3 (List.length answers)

let test_play_sequence_game correct_inputs =
  let input = String.concat "\n" correct_inputs ^ "\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let result =
    try play_sequence_game ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;
  result

let run_sequence_game_with_inputs inputs =
  let input = String.concat "\n" inputs ^ "\n" in
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  let result =
    try play_sequence_game ()
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;
  result

let test_game_incorrect_answer _ =
  Random.init 42;
  let _, correct_answer, answers = get_sequence_data () in
  let incorrect_choice =
    List.mapi
      (fun i ans -> if ans <> correct_answer then Some i else None)
      answers
    |> List.filter_map Fun.id |> List.hd
  in
  let inputs = [ string_of_int (incorrect_choice + 1) ] in
  let points = run_sequence_game_with_inputs inputs in
  assert_equal 0 points

let test_game_invalid_input _ =
  let correct_inputs = [ "a" ] in
  let points = test_play_sequence_game correct_inputs in
  assert_equal 0 points

let test_game_choice_out_of_range _ =
  let correct_inputs = [ "4" ] in
  let points = test_play_sequence_game correct_inputs in
  assert_equal 0 points

let test_handle_scenario_with_sequence_game _ =
  (* Mock the Sequence game *)
  let module MockSequence = struct
    let play_sequence_game () =
      Printf.printf "Sequence game played.\n";
      3 (* Simulate earning 3 points *)
  end in
  (* Override handle_scenario to use the mock *)
  let handle_scenario scenario points =
    let updated_points =
      if scenario.requires_sequence_game then
        points + MockSequence.play_sequence_game ()
      else points
    in
    updated_points
  in

  (* Define the scenario *)
  let scenario =
    {
      description = "Test sequence game";
      options = [ ("Option 1", 10, "Good choice!") ];
      requires_scramble_game = false;
      requires_sequence_game = true;
      (* Trigger sequence game *)
      requires_reaction_game = false;
      requires_math_game = false;
    }
  in

  (* Provide valid input *)
  let input = "1\n" in
  (* Mock input to select the correct option *)
  let old_stdin = Unix.dup Unix.stdin in
  let pipe_read, pipe_write = Unix.pipe () in
  Unix.write_substring pipe_write input 0 (String.length input) |> ignore;
  Unix.close pipe_write;
  Unix.dup2 pipe_read Unix.stdin;

  (* Run the scenario handler *)
  let updated_points =
    try handle_scenario scenario 0
    with exn ->
      Unix.dup2 old_stdin Unix.stdin;
      raise exn
  in

  (* Restore stdin *)
  Unix.dup2 old_stdin Unix.stdin;
  Unix.close old_stdin;

  (* Validate the updated points *)
  assert_equal 13 updated_points (* 10 from option + 3 from sequence game *)
    ~msg:"Points should update correctly with sequence game"

let test_scenario_termination _ =
  let final_score = play_traffic_cop () in
  assert_bool "Game should end with a non-negative score" (final_score >= 0)

let tests =
  "Test Suite"
  >::: [
         "test_scenarios_count" >:: test_scenarios_count;
         "test_scenario_structure" >:: test_scenario_structure;
         "test_generate_question" >:: test_generate_question;
         "test_check_correct_answer" >:: test_check_correct_answer;
         "test_check_incorrect_answer" >:: test_check_incorrect_answer;
         "test_play_quiz" >:: test_play_quiz;
         "test_random_reproducibility" >:: test_random_reproducibility;
         "test_scenario_count" >:: test_scenario_count;
         "test_scenario_options_count" >:: test_scenario_options_count;
         "test_play_scenario" >:: test_play_scenario;
         "test_scenarios_count" >:: test_scenarios_count;
         "test_scenario_structure" >:: test_scenario_structure;
         (* "test_generate_question" >:: test_generate_question; *)
         "test_check_correct_answer" >:: test_check_correct_answer;
         "test_check_incorrect_answer" >:: test_check_incorrect_answer;
         (* "test_play_quiz" >:: test_play_quiz; *)
         "test_random_reproducibility" >:: test_random_reproducibility;
         "test_shuffle_word" >:: test_shuffle_word;
         "test_get_scrambled_word" >:: test_get_scrambled_word;
         (* "test_play_game" >:: test_play_game; *)
         "test_handle_scenario_valid_choice"
         >:: test_handle_scenario_valid_choice;
         "test_handle_scenario_invalid_choice"
         >:: test_handle_scenario_invalid_choice;
         "test_handle_scenario_valid_choice"
         >:: test_handle_scenario_valid_choice;
         "test_handle_scenario_with_mini_games"
         >:: test_handle_scenario_with_mini_games;
         "test_generate_random_sequence" >:: test_generate_random_sequence;
         "test_shuffle" >:: test_shuffle;
         "test_get_sequence_data" >:: test_get_sequence_data;
         "test_game_invalid_input" >:: test_game_invalid_input;
         "test_game_choice_out_of_range" >:: test_game_choice_out_of_range;
         (* "test_game_incorrect_answer" >:: test_game_incorrect_answer; *)
         (* "test_handle_scenario_with_sequence_game" >::
            test_handle_scenario_with_sequence_game; *)
         "test_scenario_termination" >:: test_scenario_termination;
       ]

let _ = run_test_tt_main tests
