open Printf
open Scenariohandler

let scenarios =
  [
    create_scenario
      "You arrive at the crime scene and notice several potential leads. What \
       do you do first?"
      [
        ( "Examine the body in detail",
          2,
          "You carefully examine the body for clues, uncovering vital evidence."
        );
        ( "Question witnesses",
          1,
          "Talking to witnesses provides context but may lack crucial details."
        );
        ( "Secure the perimeter and search for clues",
          2,
          "A thorough search reveals critical evidence that was initially \
           overlooked." );
      ]
      false true false false;
    create_scenario
      "A suspect is fleeing the crime scene. What is your immediate action?"
      [
        ( "Chase them on foot",
          2,
          "Your quick reflexes allow you to catch the suspect before they \
           escape." );
        ( "Call for backup",
          1,
          "This ensures safety but might give the suspect time to escape." );
        ( "Try to cut them off using your car",
          1,
          "This strategy might work but requires precise coordination." );
      ]
      false false true false;
    create_scenario
      "A key witness is nervous and unwilling to talk or provide any further \
       information. What do you do?"
      [
        ( "Offer them protection",
          2,
          "This is the right thing to do because it abides with the law." );
        ( "Persuade them with evidence",
          1,
          "Persuading the witness is not a good idea, they should speak if \
           they want." );
        ( "Threaten to arrest them",
          -1,
          "It is illegal to threaten a witness in a crime scene because they \
           have rights allowing them to only speak if they want." );
      ]
      false false false false;
    create_scenario
      "You find a coded message at the scene. Deciphering it might reveal \
       vital information."
      [
        ( "Solve the code yourself",
          2,
          "Your analytical skills uncover critical details hidden in the code."
        );
        ( "Send it to the lab for analysis",
          1,
          "This may yield results but takes more time." );
        ( "Ignore the code and focus on other leads",
          -1,
          "You risk missing an essential piece of evidence." );
      ]
      true false false false;
    create_scenario
      "A financial record linked to the case appears encrypted. Solving the \
       puzzle could be key."
      [
        ( "Work through the math problems to decode it",
          2,
          "Your effort reveals hidden details that advance the investigation."
        );
        ( "Delegate the task to a specialist",
          1,
          "This ensures accuracy but takes longer." );
        ( "Skip the encryption and move to other tasks",
          -1,
          "You risk overlooking crucial evidence." );
      ]
      false false false true;
  ]

let play_criminal_investigator () =
  printf "Welcome to the Criminal Investigator Training Simulation!\n";
  play_scenarios scenarios 5
