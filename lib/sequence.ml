open Printf

(* Randomize a list *)
let shuffle lst =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
  let sorted = List.sort compare nd in
  List.map snd sorted

(* Generate a sequence based on a mathematical pattern *)
let generate_sequence n =
  List.init n (fun i -> int_of_float (2.0 ** float_of_int i))

(* Prepare sequence data for a question *)
let get_sequence_data () =
  let sequence = generate_sequence 5 in
  let displayed_sequence = List.rev (List.tl (List.rev sequence)) in
  let sequence_str = (String.concat ", " (List.map string_of_int displayed_sequence)) ^ ", ?" in
  let correct_answer = List.hd (List.rev sequence) in
  let distractors = [correct_answer + Random.int 5 + 1; correct_answer - Random.int 5 - 1] in
  let answers = shuffle (correct_answer :: distractors) in
  (sequence_str, correct_answer, answers)

(* Play the sequence game *)
let play_sequence_game () =
  Random.self_init (); (* Initialize randomness *)
  let rec ask_question round points =
    if round > 3 then (
      printf "Congratulations! You got all points: %d\n" points
    ) else (
      let (sequence_str, correct_answer, answers) = get_sequence_data () in
      printf "\nComplete the sequence: %s\n" sequence_str;
      printf "Options:\n";
      List.iteri (fun i ans -> printf "%d) %d\n" (i + 1) ans) answers;
      printf "Enter your choice: ";
      match read_line () with
      | exception End_of_file -> printf "No input received.\n"
      | input -> (
          try
            let choice = int_of_string input in
            let selected_answer = List.nth answers (choice - 1) in
            if selected_answer = correct_answer then (
              printf "Correct! Moving to next question...\n";
              ask_question (round + 1) (points + 1) (* Recursive call for next question *)
            ) else (
              printf "Incorrect! The correct answer was: %d\n" correct_answer;
              printf "Game over. You earned %d points.\n" points
            )
          with
          | Failure _ -> printf "Invalid input. Game over.\n"
          | Invalid_argument _ -> printf "Choice out of range. Game over.\n"
        )
    )
  in
  ask_question 1 0
