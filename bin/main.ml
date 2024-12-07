open Graphics
open Dunebuilders
open Sequence

let () =
  Random.self_init ();
  print_endline "Starting the Sequence Terminal Game...";
  (* Call the full game logic from sequence.ml *)
  Sequence.play_sequence_game ();
