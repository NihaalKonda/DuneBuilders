open Random

(* Generate a random math question and its answer *)
let generate_question () =
  let num1 = 1 + Random.int 50 in
  let num2 = 1 + Random.int 50 in
  let operator = Random.int 3 in
  match operator with
  | 0 -> Printf.sprintf "%d + %d" num1 num2, num1 + num2
  | 1 -> Printf.sprintf "%d - %d" num1 num2, num1 - num2
  | 2 -> Printf.sprintf "%d * %d" num1 num2, num1 * num2
  | _ -> failwith "Unexpected operator"

(* Ask a question and return true if the user answers correctly *)
let rec ask_question question answer =
  Printf.printf "Solve: %s = " question;
  flush stdout;
  try
    let user_answer = read_int () in
    if user_answer = answer then true
    else begin
      Printf.printf "Incorrect. The correct answer was %d. Try a new question!\n\n" answer;
      false
    end
  with Failure _ ->
    Printf.printf "Please enter a valid integer.\n";
    ask_question question answer

(* Run the math quiz *)
let run_quiz () =
  Random.self_init ();
  let rec play streak =
    if streak = 5 then
      Printf.printf "Congratulations! You answered 5 questions correctly in a row and won the game!\n"
    else begin
      let question, answer = generate_question () in
      if ask_question question answer then
        play (streak + 1)  (* Increment streak if correct *)
      else
        play 0  (* Reset streak if incorrect *)
    end
  in
  play 0

(* Main function *)
let () =
  Printf.printf "Welcome to the Math Quiz!\n";
  Printf.printf "Answer 5 math questions correctly in a row to win. Good luck!\n\n";
  run_quiz ()

