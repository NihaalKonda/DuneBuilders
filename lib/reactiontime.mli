(** Run the reaction time game.
    The game consists of 3 rounds. In each round:
    - The player sees a "Get ready..." prompt.
    - After a random delay, "NOW!" is displayed.
    - The player must press the spacebar and then enter as quickly as possible.
    
    Points awarded per round:
    - < 200 ms: 3 points
    - 200 ms - 400 ms: 2 points
    - 400 ms - 600 ms: 1 point
    - > 600 ms or wrong input: 0 points
    
    Any input other than exactly a single space character (e.g. pressing another key),
    or pressing too early (input before "NOW!" is shown) results in 0 points for that round.
    
    After 3 rounds, the final score is printed.
*)

val play_reaction_game : unit -> int
