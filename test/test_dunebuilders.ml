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

let test_play_game _ =
  let input = "arrest\n" ^ "wrong_answer\n" ^ "patrol\n" in
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
         "test_generate_question" >:: test_generate_question;
         "test_check_correct_answer" >:: test_check_correct_answer;
         "test_check_incorrect_answer" >:: test_check_incorrect_answer;
         "test_play_quiz" >:: test_play_quiz;
         "test_random_reproducibility" >:: test_random_reproducibility;
         "test_shuffle_word" >:: test_shuffle_word;
         "test_get_scrambled_word" >:: test_get_scrambled_word;
         "test_play_game" >:: test_play_game;
         "test_generate_random_sequence" >:: test_generate_random_sequence;
         "test_shuffle" >:: test_shuffle;
         "test_get_sequence_data" >:: test_get_sequence_data;
         "test_game_invalid_input" >:: test_game_invalid_input;
         "test_game_choice_out_of_range" >:: test_game_choice_out_of_range;
         "test_game_incorrect_answer" >:: test_game_incorrect_answer;
       ]

let _ = run_test_tt_main tests
