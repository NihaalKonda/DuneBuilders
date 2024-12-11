open Printf

let shuffle_word word =
  let chars = List.init (String.length word) (String.get word) in
  let shuffled_chars =
    List.map snd
      (List.sort compare (List.map (fun c -> (Random.bits (), c)) chars))
  in
  String.concat "" (List.map (String.make 1) shuffled_chars)

let words =
  [
    "arrest";
    "justice";
    "investigation";
    "patrol";
    "detective";
    "evidence";
    "warrant";
    "forensics";
    "suspect";
    "crime";
  ]

let rec get_scrambled_word used_words =
  let remaining_words =
    List.filter (fun word -> not (List.mem word used_words)) words
  in
  if remaining_words = [] then failwith "No more words available"
  else
    let word =
      List.nth remaining_words (Random.int (List.length remaining_words))
    in
    (shuffle_word word, word)

let play_game () =
  Random.self_init ();
  let rec aux remaining_rounds points used_words =
    if remaining_rounds = 0 then
      printf "\nGame over! You scored %d points.\n" points
    else
      let scrambled, correct_word = get_scrambled_word used_words in
      printf "\nUnscramble the word: %s\n" scrambled;
      printf "Enter your answer: ";
      match read_line () with
      | exception End_of_file -> printf "No input received. Exiting game.\n"
      | input ->
          if String.lowercase_ascii input = correct_word then (
            printf "Correct!\n";
            aux (remaining_rounds - 1) (points + 1) (correct_word :: used_words))
          else (
            printf "Incorrect! The correct word was: %s\n" correct_word;
            aux (remaining_rounds - 1) points (correct_word :: used_words))
  in
  aux 3 0 []
