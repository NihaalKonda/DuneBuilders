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
    create_scenario
      "You find a student passed out near the campus library late at night. \
       What will you do?"
      [
        ( "Check their condition and call emergency services",
          2,
          "A prompt and responsible course of action." );
        ( "Escort them back to their dorm and suggest rest",
          1,
          "Helpful, but misses a chance to ensure proper care." );
        ( "Ignore the situation and continue patrolling",
          -2,
          "Neglectful and potentially harmful." );
      ]
      false false false false;
    create_scenario
      "You notice a car driving recklessly through the campus parking lot. \
       What is your response?"
      [
        ( "Stop the vehicle and issue a citation",
          2,
          "Enforces campus safety and discourages reckless driving." );
        ( "Take down the license plate and report the incident",
          1,
          "Ensures accountability, but allows the driver to leave." );
        ( "Ignore it, as no one seems to be in danger",
          -1,
          "Fails to address a serious safety concern." );
      ]
      false false false true;
    create_scenario
      "A fire alarm goes off in a dorm building, and students are panicking. \
       What will you do?"
      [
        ( "Ensure everyone evacuates and assist the fire department",
          2,
          "Promotes safety and coordinates emergency response." );
        ( "Attempt to locate the source of the alarm",
          1,
          "Proactive, but could put you at unnecessary risk." );
        ( "Ignore the alarm, assuming it’s a false alarm",
          -2,
          "Neglects the possibility of a real emergency." );
      ]
      true false false false;
  ]

let play_campus_police () =
  printf "Welcome to the Campus Police Training Simulation!\n";
  play_scenarios scenarios 5
