open Random

let generate_question () =
  let num1 = 1 + Random.int 50 in
  let num2 = 1 + Random.int 50 in
  let operator = Random.int 3 in
  match operator with
  | 0 -> (Printf.sprintf "%d + %d" num1 num2, num1 + num2)
  | 1 -> (Printf.sprintf "%d - %d" num1 num2, num1 - num2)
  | 2 -> (Printf.sprintf "%d * %d" num1 num2, num1 * num2)
  | _ -> failwith "Unexpected operator"

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
  Random.self_init ();
  let rec play questions_left points =
    if questions_left = 0 then (
      Printf.printf "\nQuiz over! You scored %d points.\n" points;
      points)
    else
      let question, answer = generate_question () in
      let points_earned = ask_question question answer in
      play (questions_left - 1) (points + points_earned)
  in
  play 5 0
