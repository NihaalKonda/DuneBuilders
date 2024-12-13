open Printf
open Scenariohandler

let scenarios =
  [
    create_scenario
      "You've recovered a crucial piece of evidence at a crime scene - a \
       single fingerprint. The database search yields no matches. What's your \
       next course of action?"
      [
        ( "Ignore the fingerprint as it doesn't lead to any immediate suspects.",
          -1,
          "Discarding valuable evidence hinders the investigation and \
           potential apprehension of the perpetrator." );
        ( "Expand the search parameters in the database and cross-reference \
           with known associates of potential suspects.",
          2,
          "This proactive approach increases the chances of identifying a \
           match and advancing the investigation." );
        ( "Focus solely on other investigative avenues, as the fingerprint is \
           likely a dead end.",
          -1,
          "Overlooking potential leads, even seemingly insignificant ones, can \
           compromise a thorough investigation." );
      ]
      false false false false;
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
      "You're interrogating a suspect who is providing inconsistent and \
       evasive answers. How do you proceed?"
      [
        ( "Immediately confront the suspect with the inconsistencies and \
           accuse them of lying.",
          -1,
          "An accusatory approach can escalate the situation and make the \
           suspect less cooperative." );
        ( "Maintain a calm and non-confrontational demeanor, patiently probing \
           for further details and clarifying inconsistencies.",
          2,
          "A patient and understanding approach can build rapport and \
           encourage the suspect to provide more truthful information." );
        ( "End the interrogation and focus on gathering evidence from other \
           sources.",
          1,
          "While gathering evidence from other sources is crucial, prematurely \
           abandoning an interrogation may miss vital information." );
      ]
      false false false false;
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
      "You receive an anonymous tip regarding the location of a concealed \
       weapon. How do you proceed"
      [
        ( "Disregard the tip as it's anonymous and potentially unreliable.",
          -1,
          "Dismissing credible leads, even anonymous ones, can hinder the \
           investigation and potentially endanger public safety." );
        ( "Immediately dispatch a team to investigate the location while \
           discreetly attempting to verify the tip's source.",
          2,
          "This cautious approach balances the need for immediate action with \
           the importance of verifying the information." );
        ( "Publicly announce the tip to gather further information from the \
           community.",
          -1,
          "Publicly disclosing sensitive information related to an ongoing \
           investigation can compromise the operation and potentially alert \
           the suspect." );
      ]
      false false false false;
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
