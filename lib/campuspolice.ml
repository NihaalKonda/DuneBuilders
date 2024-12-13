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
      "You receive a report of a possible active shooter situation on campus. \
       What is your immediate priority?"
      [
        ( "Secure the perimeter and evacuate students and staff from the area.",
          2,
          "Prioritizing the safety of individuals on campus is paramount in an \
           active shooter situation." );
        ( "Immediately apprehend the suspect.",
          -1,
          "Prioritizing immediate apprehension could put officers and \
           civilians at unnecessary risk." );
        ( "Gather information and assess the situation from a distance.",
          -1,
          "Hesitation in an active shooter situation can lead to increased \
           casualties." );
      ]
      false false false false;
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
      "A student reports experiencing cyberbullying and harassment online. How \
       do you approach this situation?"
      [
        ( "Dismiss the report as online harassment is not a physical threat.",
          -1,
          "Cyberbullying can have severe psychological and emotional impacts \
           on victims." );
        ( "Document the incident and offer support resources to the student.",
          1,
          "While documenting the incident is important, offering support \
           resources is equally crucial." );
        ( "Investigate the incident, gather evidence, and potentially pursue \
           disciplinary action against the perpetrator.",
          2,
          "A comprehensive response includes both support for the victim and \
           accountability for the perpetrator." );
      ]
      false false false false;
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
