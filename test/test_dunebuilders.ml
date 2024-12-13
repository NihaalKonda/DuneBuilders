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
       ]

let _ = run_test_tt_main tests
