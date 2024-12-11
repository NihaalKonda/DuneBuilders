open Printf
open Scenariohandler

let scenarios =
  [
    create_scenario
      "You spot an individual lingering near the border fence after dusk.\n\
       They appear nervous and are avoiding well-lit areas."
      [
        ( "Approach the individual and question them",
          2,
          "By calmly approaching and inquiring, you gather information and \
           possibly prevent illegal entry without escalating the situation." );
        ( "Call for immediate backup and surround the area",
          1,
          "This might ensure safety but could cause unnecessary alarm if the \
           individual is not a threat." );
        ( "Ignore the individual and move on to another patrol point",
          -1,
          "This may allow potential illegal activity to go unchecked." );
      ]
      false false false false;
    create_scenario
      "A suspicious vehicle approaches the checkpoint.\n\
       You notice the driver is acting unusually nervous and the passenger \
       looks tense."
      [
        ( "Ask the driver to step out and conduct a thorough search",
          2,
          "A direct, legal search could reveal contraband and ensure the \
           integrity of the checkpoint." );
        ( "Let the vehicle pass after a quick ID check",
          -2,
          "This risks missing potential contraband or illegal items." );
        ( "Request additional identification and verify vehicle documentation \
           thoroughly",
          1,
          "This is a safer approach than waving them through, but not as \
           proactive as searching immediately." );
      ]
      true false false false;
    create_scenario
      "You receive a tip about a hidden compartment in a cargo truck carrying \
       fresh produce.\n\
       The driver has proper documentation but seems evasive when questioned \
       about the cargo."
      [
        ( "Perform a detailed inspection of the cargo and vehicle",
          2,
          "Thoroughly checking the cargo can prevent illegal items from \
           passing through undetected." );
        ( "Take the driver's word and expedite the truck to keep traffic flowing",
          -1,
          "This could allow contraband to enter, compromising border security."
        );
        ( "Call for a canine unit to aid in the inspection",
          1,
          "A canine unit can assist without causing unnecessary delay if the \
           driver is compliant." );
      ]
      false false false false;
    create_scenario
      "As you talk to a person at the checkpoint, they suddenly attempt to run.\n\
       You must react quickly to subdue them safely."
      [
        ( "Attempt to grab them immediately",
          2,
          "A quick, decisive action might prevent escape, but you must be \
           quick and accurate." );
        ( "Call for help first",
          1,
          "This might be safer, but could give them time to flee." );
        ( "Do nothing and observe their movements",
          -1,
          "This gives the suspect time to escape and is not ideal." );
      ]
      false true false false;
    create_scenario
      "A car is parked illegally in a restricted area near the border.\n\
       While investigating, you discover a hidden document that appears \
       scrambled. Solving this puzzle may reveal crucial information."
      [
        ( "Solve the puzzle to uncover more details",
          2,
          "By solving the scrambled puzzle, you ensure you don't miss \
           important clues." );
        ( "Ignore the puzzle and have the car towed",
          -1,
          "This resolves the immediate issue but you might miss critical \
           information." );
        ( "Call for help to solve the puzzle",
          1,
          "Cautious but slower, you might still learn something useful." );
      ]
      false false true false;
    create_scenario
      "You're reviewing suspicious financial records tied to a smuggling ring.\n\
       To access the encrypted files, you must solve a series of math problems \
       to unlock the data."
      [
        ( "Solve the math puzzles to gain access",
          2,
          "Demonstrating your analytical skills can help you uncover hidden \
           operations." );
        ( "Ignore the math puzzles and move on",
          -1,
          "You'll miss potentially critical intelligence." );
        ( "Call for a specialist in financial crimes",
          1,
          "Might help, but delays the investigation." );
      ]
      false false false true;
  ]

let play_border_patrol () =
  printf "Welcome to the Border Patrol Training Simulation!\n";
  play_scenarios scenarios 5
