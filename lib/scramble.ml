open Printf

let get_word used_words =
  let fixed_order = [ "arrest"; "justice"; "patrol" ] in
  let remaining_words =
    List.filter (fun word -> not (List.mem word used_words)) fixed_order
  in
  match remaining_words with
  | [] -> failwith "No more words available"
  | word :: _ -> (word, word)

let play_game () =
  let rec aux remaining_rounds points used_words =
    if remaining_rounds = 0 then (
      printf "\nGame over! You scored %d points.\n" points;
      points)
    else
      let scrambled, correct_word = get_word used_words in
      printf "\nGuess the word: %s\n" scrambled;
      printf "Enter your answer: ";
      match read_line () with
      | exception End_of_file ->
          printf "No input received. Exiting game.\n";
          points
      | input ->
          if String.lowercase_ascii input = correct_word then (
            printf "Correct!\n";
            aux (remaining_rounds - 1) (points + 1) (correct_word :: used_words))
          else (
            printf "Incorrect! The correct word was: %s\n" correct_word;
            aux (remaining_rounds - 1) points (correct_word :: used_words))
  in
  aux 3 0 []
