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

let test_create_scenario _ =
  (* Create a scenario *)
  let scenario =
    create_scenario "Test scenario"
      [ ("Option 1", 2, "Explanation 1"); ("Option 2", -1, "Explanation 2") ]
      false false false false
  in
  (* Test that it integrates correctly with `handle_scenario` *)
  let points_after_choice = handle_scenario scenario 5 in
  assert_equal points_after_choice
    7 (* Assuming the default choice adds 2 points *)

let test_play_scenarios _ =
  (* Create a list of scenarios *)
  let scenarios =
    [
      create_scenario "Scenario 1"
        [ ("Option 1", 2, "Explanation 1"); ("Option 2", -1, "Explanation 2") ]
        false false false false;
      create_scenario "Scenario 2"
        [ ("Option A", 1, "Explanation A"); ("Option B", -2, "Explanation B") ]
        false false false false;
    ]
  in
  (* Test playing through the scenarios *)
  let final_score = play_scenarios scenarios 5 in
  assert_bool "Final score should be >= 0" (final_score >= 0)

let tests =
  "suite"
  >::: [
         "Test create_scenario" >:: test_create_scenario;
         "Test play_scenarios" >:: test_play_scenarios;
       ]

let _ = run_test_tt_main tests
