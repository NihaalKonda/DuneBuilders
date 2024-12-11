open Printf
open Scenariohandler

let scenarios =
  [
    create_scenario
      "You receive a call about excessive noise in a dorm. What will you do?"
      [
        ( "Knock and speak with the residents",
          2,
          "Professional and effective response." );
        ("Call for backup", 1, "Safe, but might escalate unnecessarily.");
        ("Issue citations immediately", -1, "Aggressive and confrontational.");
      ]
      false false false false;
    create_scenario
      "You find an unmarked package in a dormitory. It might be dangerous."
      [
        ( "Call the bomb squad",
          2,
          "Ensures safety with professional intervention." );
        ( "Evacuate and secure the area",
          1,
          "Safety first, but delays expert assistance." );
        ("Ignore it and continue patrolling", -2, "Reckless and risky decision.");
      ]
      false true false false;
    create_scenario
      "You witness a heated argument near the campus cafeteria escalating into \
       a fight."
      [
        ("Intervene immediately", 2, "De-escalates the situation effectively.");
        ("Call for backup", 1, "Safe, but delays resolution.");
        ("Walk away", -1, "Neglectful and irresponsible.");
      ]
      false false true false;
  ]

let play_campus_police () =
  printf "Welcome to the Campus Police Training Simulation!\n";
  play_scenarios scenarios 5
