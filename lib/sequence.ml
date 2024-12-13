open Printf

let sequences =
  [
    ([ 30; 28; 25; 21; 16 ], 10);
    ([ 2; 4; 8; 16; 32 ], 64);
    ([ 75; 15; 25; 5; 15 ], 3);
  ]

let shuffle lst =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sorted = List.sort compare nd in
  List.map snd sorted

let play_sequence_game () =
  let rec ask_question round points =
    if round > List.length sequences then (
      printf "Congratulations! You got all points: %d\n" points;
      points)
    else
      let sequence, correct_answer = List.nth sequences (round - 1) in
      let displayed_sequence = List.rev (List.tl (List.rev sequence)) in
      let sequence_str =
        String.concat ", " (List.map string_of_int displayed_sequence) ^ ", ?"
      in
      let distractors = [ correct_answer + 2; correct_answer - 2 ] in
      let answers = shuffle (correct_answer :: distractors) in
      printf "\nComplete the sequence: %s\n" sequence_str;
      printf "Options:\n";
      List.iteri (fun i ans -> printf "%d) %d\n" (i + 1) ans) answers;
      printf "Enter your choice: ";
      match read_line () with
      | exception End_of_file ->
          printf "No input received. Exiting game.\n";
          points
      | input -> (
          try
            let choice = int_of_string input in
            if choice < 1 || choice > List.length answers then (
              printf "Choice out of range. Game over.\n";
              points)
            else
              let selected_answer = List.nth answers (choice - 1) in
              if selected_answer = correct_answer then (
                if round = List.length sequences then
                  printf "Correct! You have completed the game.\n"
                else printf "Correct! Moving to next question...\n";
                ask_question (round + 1) (points + 1))
              else (
                printf "Incorrect! The correct answer was: %d\n" correct_answer;
                printf "Game over. You earned %d points.\n" points;
                points)
          with Failure _ ->
            printf "Invalid input. Game over.\n";
            points)
  in
  ask_question 1 0
