open Printf
open Random

let remaining_questions = ref []

let shuffle_list lst =
  let arr = Array.of_list lst in
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let initialize_questions () =
  let questions =
    [
      ("We need backup at the scene immediately.", "urgent");
      ("The situation is under control.", "calm");
      ("The suspect is armed and dangerous.", "threat");
      ("We have apprehended the suspect.", "resolved");
      ("The victim requires medical attention immediately.", "urgent");
    ]
  in
  remaining_questions := shuffle_list questions

let generate_sentiment_question () =
  match !remaining_questions with
  | [] ->
      initialize_questions ();
      let hd = List.hd !remaining_questions in
      remaining_questions := List.tl !remaining_questions;
      hd
  | hd :: tl ->
      remaining_questions := tl;
      hd

let rec ask_sentiment_question sentence correct_category =
  printf "\nHere's the sentence: \"%s\"\n" sentence;
  printf "Classify the situation (urgent/calm/threat/resolved): ";
  flush stdout;
  try
    let user_input = read_line () in
    if String.lowercase_ascii user_input = correct_category then (
      printf "Correct!\n\n";
      2)
    else (
      printf "Wrong. The correct answer was: %s. No points awarded.\n\n"
        correct_category;
      0)
  with Failure _ ->
    printf "Invalid input. Please try again.\n";
    ask_sentiment_question sentence correct_category

let play_sentiment_game () =
  Random.self_init ();
  initialize_questions ();
  let rec play questions_left points =
    if questions_left = 0 then (
      printf "\nGame over! You scored %d points.\n" points;
      points)
    else
      let sentence, correct_category = generate_sentiment_question () in
      let points_earned = ask_sentiment_question sentence correct_category in
      play (questions_left - 1) (points + points_earned)
  in
  play 5 0
