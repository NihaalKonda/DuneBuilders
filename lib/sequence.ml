open Printf

let shuffle lst =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sorted = List.sort compare nd in
  List.map snd sorted

let generate_random_sequence n =
  Random.self_init ();
  let start = Random.int 10 + 1 in
  let step = Random.int 5 + 1 in
  List.init n (fun i -> start + (i * step))

let get_sequence_data () =
  let sequence = generate_random_sequence 5 in
  let displayed_sequence = List.rev (List.tl (List.rev sequence)) in
  let sequence_str =
    String.concat ", " (List.map string_of_int displayed_sequence) ^ ", ?"
  in
  let correct_answer = List.hd (List.rev sequence) in
  let distractors =
    [ correct_answer + Random.int 5 + 1; correct_answer - Random.int 5 - 1 ]
  in
  let answers = shuffle (correct_answer :: distractors) in
  (sequence_str, correct_answer, answers)

let play_sequence_game () =
  Random.self_init ();
  let rec ask_question round points =
    if round > 3 then (
      printf "Congratulations! You got all points: %d\n" points;
      points)
    else
      let sequence_str, correct_answer, answers = get_sequence_data () in
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
            let selected_answer = List.nth answers (choice - 1) in
            if selected_answer = correct_answer then (
              if round = 3 then printf "Correct! You have completed the game.\n"
              else printf "Correct! Moving to next question...\n";
              ask_question (round + 1) (points + 1))
            else (
              printf "Incorrect! The correct answer was: %d\n" correct_answer;
              printf "Game over. You earned %d points.\n" points;
              points)
          with
          | Failure _ ->
              printf "Invalid input. Game over.\n";
              points
          | Invalid_argument _ ->
              printf "Choice out of range. Game over.\n";
              points)
  in
  ask_question 1 0
