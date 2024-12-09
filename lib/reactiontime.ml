open Printf
open Unix

(* A safe_select function that retries if select is interrupted by EINTR *)
let safe_select r w e t =
  let rec aux () =
    try select r w e t with Unix.Unix_error (Unix.EINTR, _, _) -> aux ()
  in
  aux ()

(* Helper function to get a random delay between min_sec and max_sec *)
let random_delay min_sec max_sec = Random.float (max_sec -. min_sec) +. min_sec

(* Flush any buffered input lines before starting a round *)
let flush_stdin () =
  let in_descr = Unix.descr_of_in_channel Stdlib.stdin in
  let rec loop () =
    let readable, _, _ = safe_select [ in_descr ] [] [] 0.0 in
    if readable <> [] then
      (* There is input available, read and discard it *)
      try
        ignore (Stdlib.input_line Stdlib.stdin);
        loop () (* Check again if more input is waiting *)
      with End_of_file -> () (* No more input to read *)
    else ()
  in
  loop ()

(* Measure reaction time *)
let play_round () =
  (* Flush at the start of the round to avoid leftover input from previous
     rounds *)
  flush_stdin ();

  printf "Get ready...\n";
  Stdlib.flush Stdlib.stdout;

  (* Sleep for a random delay between 1 and 3 seconds *)
  let delay = random_delay 1.0 3.0 in
  Unix.sleepf delay;

  (* Print NOW and record time *)
  printf "NOW!\n";
  Stdlib.flush Stdlib.stdout;
  let start_time = Unix.gettimeofday () in

  (* Use safe_select to wait up to 1 second for input *)
  let in_descr = Unix.descr_of_in_channel Stdlib.stdin in
  let timeout = 1.0 in
  let readable, _, _ = safe_select [ in_descr ] [] [] timeout in

  if readable = [] then (
    (* No input within 1 second *)
    printf "Too slow! 0 points.\n";
    0)
  else
    (* Input is available, read it immediately *)
    let input = Stdlib.input_line Stdlib.stdin in
    let end_time = Unix.gettimeofday () in
    let diff_ms = (end_time -. start_time) *. 1000.0 in

    if input = "" then
      if diff_ms < 10.0 then (
        (* If reaction time is extremely short, user pressed Enter before NOW
           was printed *)
        printf "You clicked it too soon!\n";
        0)
      else if diff_ms < 200.0 then 3
      else if diff_ms < 400.0 then 2
      else if diff_ms < 600.0 then 1
      else 0
    else (* Wrong input *)
      0

let play_reaction_game () =
  Random.self_init ();
  let rounds = 3 in
  let rec loop n acc =
    if n = 0 then acc
    else
      let points = play_round () in
      printf "You earned %d points this round.\n\n" points;
      Stdlib.flush Stdlib.stdout;
      loop (n - 1) (acc + points)
  in
  let total_points = loop rounds 0 in
  printf "Game over. Your total score is %d\n" total_points;
  Stdlib.flush Stdlib.stdout
