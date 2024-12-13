let questions =
  [ ("5 + 3", 8); ("10 - 7", 3); ("4 * 6", 24); ("9 + 2", 11); ("8 - 3", 5) ]

let rec ask_question question answer =
  Printf.printf "Solve: %s = " question;
  flush stdout;
  try
    let user_answer = read_int () in
    if user_answer = answer then (
      Printf.printf "Correct!\n\n";
      1)
    else (
      Printf.printf
        "Incorrect. The correct answer was %d. No points awarded.\n\n" answer;
      0)
  with Failure _ ->
    Printf.printf "Please enter a valid integer.\n";
    ask_question question answer

let play_quiz () =
  let rec play questions points =
    match questions with
    | [] ->
        Printf.printf "\nQuiz over! You scored %d points.\n" points;
        points
    | (question, answer) :: rest ->
        let points_earned = ask_question question answer in
        play rest (points + points_earned)
  in
  play questions 0
