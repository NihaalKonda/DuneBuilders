(* Generate a simple doubling sequence *)
let generate_sequence n =
  List.init n (fun i -> int_of_float (2.0 ** float_of_int i))

let get_sequence_data () =
  let sequence = generate_sequence 5 in
  (* Remove the last element from display to create the "?" sequence *)
  let displayed_sequence = List.rev (List.tl (List.rev sequence)) in
  let sequence_str = (String.concat ", " (List.map string_of_int displayed_sequence)) ^ ", ?" in
  let correct_answer = List.hd (List.rev sequence) in
  let answers = [correct_answer; correct_answer + 1; correct_answer - 1] in
  let shuffled_answers = List.sort compare answers in
  (sequence_str, correct_answer, shuffled_answers)

let play_sequence_game () =
  let (sequence_str, correct_answer, shuffled_answers) = get_sequence_data () in
  print_endline ("Complete the sequence: " ^ sequence_str);
  print_endline "Options:";
  List.iteri (fun i ans ->
    print_endline (string_of_int (i+1) ^ ") " ^ string_of_int ans)
  ) shuffled_answers;

  print_string "Enter the number corresponding to your choice: ";
  match read_line () with
  | exception End_of_file ->
      print_endline "No input received. Exiting.";
  | input ->
      (try
         let choice_index = int_of_string input - 1 in
         if choice_index < 0 || choice_index >= List.length shuffled_answers then
           print_endline "Invalid choice!"
         else
           let chosen_answer = List.nth shuffled_answers choice_index in
           if chosen_answer = correct_answer then
             print_endline "Correct!"
           else
             print_endline "Incorrect!"
       with Failure _ ->
         print_endline "Please enter a valid number!")

(* Entry point for testing *)
let () =
  print_endline "Welcome to the Sequence Terminal Game!";
  play_sequence_game ();
