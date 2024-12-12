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

(* Test cases *)
let test_scenarios_count _ =
  assert_equal 6 (List.length scenarios) ~msg:"Incorrect number of scenarios"

let test_scenario_structure _ =
  let first_scenario = List.hd scenarios in
  assert_equal 3
    (List.length (get_scenario_options first_scenario))
    ~msg:"Incorrect number of options in the first scenario"

(* Test suite *)
let tests =
  "Border Patrol Test Suite"
  >::: [
         "test_scenarios_count" >:: test_scenarios_count;
         "test_scenario_structure" >:: test_scenario_structure;
       ]
(* Run the tests *)

let _ = run_test_tt_main tests
