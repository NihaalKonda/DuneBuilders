open Printf
open Scenariohandler

let scenarios =
  [
    create_scenario
      "You receive a call about a driver weaving erratically through traffic. \
       You need to act quickly."
      [
        ( "Pull the driver over for questioning",
          2,
          "This is the safest and most responsible action." );
        ( "Ignore the call and continue patrolling",
          -1,
          "Neglecting this can lead to a dangerous situation." );
        ( "Request additional backup before proceeding",
          1,
          "This ensures safety but delays action." );
      ]
      false false false false;
    create_scenario
      "A car is parked illegally in a handicapped spot without a permit. While \
       investigating, you find a clue that requires solving a scrambled word \
       puzzle to verify parking records."
      [
        ( "Solve the puzzle to uncover more details",
          2,
          "This ensures you investigate thoroughly and find the necessary \
           information." );
        ( "Ignore the puzzle and tow the car",
          -1,
          "This resolves the issue but might miss important information." );
        ( "Call for help to solve the puzzle",
          1,
          "This is cautious but slows down the investigation." );
      ]
      true false false false;
    create_scenario
      "While monitoring traffic, you stop a vehicle with mismatched plates. \
       Running the plates triggers a system alert requiring you to solve a \
       pattern sequence to unlock detailed vehicle information."
      [
        ( "Solve the sequence to proceed",
          2,
          "This helps you investigate the potential vehicle mismatch quickly."
        );
        ( "Let the driver go with a warning",
          -1,
          "This neglects your duty to investigate properly." );
        ( "Call for backup and delay the investigation",
          1,
          "This ensures safety but slows down the process." );
      ]
      false true false false;
  ]

let play_traffic_cop () =
  printf "Welcome to the Traffic Cop Training Simulation!\n";
  play_scenarios scenarios 5
