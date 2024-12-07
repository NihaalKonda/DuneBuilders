open Graphics
open Dunebuilders
open Sequence
let test_sequence_in_main () =
  print_endline "Testing Sequence Game in Terminal...";
  (* Generate sequence data *)
  let (sequence_str, correct_answer, shuffled_answers) = get_sequence_data () in

  (* Display the sequence *)
  print_endline ("Complete the sequence: " ^ sequence_str);
  print_endline "Options:";
  List.iteri (fun i ans -> Printf.printf "%d) %d\n" (i + 1) ans) shuffled_answers;

  (* Get user input *)
  print_string "Enter your choice: ";
  match read_line () with
  | exception End_of_file -> print_endline "No input received."
  | input ->
      (try
         let choice = int_of_string input in
         let selected_answer = List.nth shuffled_answers (choice - 1) in
         if selected_answer = correct_answer then
           print_endline "Correct!"
         else
           print_endline "Incorrect!"
       with
       | Failure _ -> print_endline "Invalid input."
       | Invalid_argument _ -> print_endline "Choice out of range.")

let () =
Random.self_init (); (* Initialize randomness once at the start *)
test_sequence_in_main ();
