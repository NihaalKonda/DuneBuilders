open Graphics
open Campuspolice
open Trafficcop
open Borderpatrol
open Criminalinvestigator

(* Define the type representing the game states *)
type game_state =
  | Welcome
  | RoleSelection
  | FinalScreen of (string * int) list (* List of roles and scores *)

(* GUI Constants *)
let window_width = 600
let window_height = 400
let button_width = 200
let button_height = 50

(* Utility to draw a button *)
let draw_button x y width height text =
  set_color blue;
  fill_rect x y width height;
  set_color white;
  let text_x = x + ((width - (String.length text * 6)) / 2) in
  let text_y = y + (height / 2) - 8 in
  moveto text_x text_y;
  draw_string text

(* Display the welcome screen *)
let display_welcome_message () =
  clear_graph ();
  let text_x =
    (window_width - (String.length "Welcome to the Police Training Game!" * 6))
    / 2
  in
  let text_y = (window_height / 2) + 100 in
  moveto text_x text_y;
  set_color black;
  draw_string "Welcome to the Police Training Game!";
  let start_button_x = (window_width - button_width) / 2 in
  let start_button_y = (window_height - button_height) / 2 in
  draw_button start_button_x start_button_y button_width button_height "Start";
  synchronize ()

(* Display the role selection screen *)
let display_role_selection () =
  clear_graph ();
  let title_x = (window_width - (String.length "Choose Your Role:" * 6)) / 2 in
  let title_y = window_height - 100 in
  moveto title_x title_y;
  set_color black;
  draw_string "Choose Your Role:";
  let roles =
    [ "Campus Police"; "Criminal Investigator"; "Traffic Cop"; "Border Patrol" ]
  in
  List.iteri
    (fun i role ->
      let spacing = 70 in
      let y =
        (window_height / 2) + (spacing * (List.length roles / 2)) - (i * spacing)
      in
      draw_button
        ((window_width - button_width) / 2)
        y button_width button_height role)
    roles;
  synchronize ()

(* Display the final screen *)
let display_final_screen completed_roles =
  clear_graph ();
  set_color black;
  let title_x =
    (window_width - (String.length "Game Over! Final Scores:" * 6)) / 2
  in
  moveto title_x (window_height - 50);
  draw_string "Game Over! Final Scores:";
  List.iteri
    (fun i (role, score) ->
      let text_x = 50 in
      let text_y = window_height - (100 + (i * 30)) in
      moveto text_x text_y;
      draw_string (Printf.sprintf "%s: %d points" role score))
    completed_roles;
  let restart_x = (window_width - button_width) / 2 in
  let restart_y = 50 in
  draw_button restart_x restart_y button_width button_height "Restart";
  synchronize ()

(* Determine which role is clicked based on coordinates *)
let handle_role_click x y =
  let roles =
    [ "Campus Police"; "Criminal Investigator"; "Traffic Cop"; "Border Patrol" ]
  in
  let start_y = (window_height / 2) + (70 * (List.length roles / 2)) in
  List.mapi
    (fun i role ->
      let button_x = (window_width - button_width) / 2 in
      let button_y = start_y - (i * 70) in
      (role, button_x, button_y))
    roles
  |> List.find_opt (fun (_, bx, by) ->
         x >= bx && x <= bx + button_width && y >= by && y <= by + button_height)
  |> Option.map (fun (r, _, _) -> r)

(* Play the role's game and return the score *)
let play_role_game role =
  close_graph ();
  let role_score =
    match role with
    | "Campus Police" -> play_campus_police ()
    | "Criminal Investigator" -> play_criminal_investigator ()
    | "Traffic Cop" -> play_traffic_cop ()
    | "Border Patrol" -> play_border_patrol ()
    | _ -> 0
  in
  Graphics.open_graph (Printf.sprintf " %dx%d" window_width window_height);
  (role, role_score)

(* Main game loop *)
let rec game_loop state completed_roles =
  match state with
  | Welcome ->
      display_welcome_message ();
      let event = wait_next_event [ Button_down ] in
      if event.button then
        let start_button_x = (size_x () - button_width) / 2 in
        let start_button_y = (size_y () - button_height) / 2 in
        if
          event.mouse_x >= start_button_x
          && event.mouse_x <= start_button_x + button_width
          && event.mouse_y >= start_button_y
          && event.mouse_y <= start_button_y + button_height
        then game_loop RoleSelection completed_roles
        else game_loop Welcome completed_roles
  | RoleSelection ->
      display_role_selection ();
      let event = wait_next_event [ Button_down ] in
      if event.button then
        match handle_role_click event.mouse_x event.mouse_y with
        | Some selected_role ->
            let role, score = play_role_game selected_role in
            let updated_roles = (role, score) :: completed_roles in
            if List.length updated_roles = 4 then
              game_loop (FinalScreen updated_roles) updated_roles
            else game_loop RoleSelection updated_roles
        | None -> game_loop RoleSelection completed_roles
      else game_loop RoleSelection completed_roles
  | FinalScreen completed_roles ->
      display_final_screen completed_roles;
      let event =
        Graphics.wait_next_event [ Graphics.Button_down; Graphics.Key_pressed ]
      in
      let restart_x = (window_width - button_width) / 2 in
      let restart_y = 50 in
      if event.button then
        if
          event.mouse_x >= restart_x
          && event.mouse_x <= restart_x + button_width
          && event.mouse_y >= restart_y
          && event.mouse_y <= restart_y + button_height
        then game_loop Welcome []
        else game_loop (FinalScreen completed_roles) completed_roles
      else if event.key = 'q' then (
        Graphics.close_graph ();
        print_endline "Thank you for playing!")
      else game_loop Welcome []

(* Initialize the GUI window *)
let initialize_gui () =
  Graphics.open_graph (Printf.sprintf " %dx%d" window_width window_height);
  Graphics.auto_synchronize false;
  Graphics.set_window_title "Police Training Game";
  game_loop Welcome []

(* Start the game *)
let () = initialize_gui ()
