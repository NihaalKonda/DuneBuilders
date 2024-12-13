open Graphics
open Campuspolice
open Trafficcop
open Borderpatrol
open Criminalinvestigator

type game_state =
  | Welcome
  | RoleSelection
  | RoleSelected of string
  | RoleComplete of (string * int) (* New state after finishing one role *)
  | FinalScreen of (string * int) list

let window_width = 600
let window_height = 400
let button_width = 200
let button_height = 50

let draw_button x y width height text =
  set_color blue;
  fill_rect x y width height;
  set_color white;
  let text_x = x + ((width - (String.length text * 6)) / 2) in
  let text_y = y + (height / 2) - 8 in
  moveto text_x text_y;
  draw_string text

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

let display_role_selection () =
  clear_graph ();
  let title = "Choose Your Role:" in
  let title_x = (window_width - (String.length title * 6)) / 2 in
  let title_y = window_height - 100 in
  moveto title_x title_y;
  set_color black;
  draw_string title;

  let roles =
    [ "Campus Police"; "Criminal Investigator"; "Traffic Cop"; "Border Patrol" ]
  in
  let total_height =
    (List.length roles * button_height) + ((List.length roles - 1) * 20)
  in
  let start_y = (window_height - total_height) / 2 in

  List.iteri
    (fun i role ->
      let button_x = (window_width - button_width) / 2 in
      let button_y = start_y + (i * (button_height + 20)) in
      draw_button button_x button_y button_width button_height role)
    roles;
  synchronize ()

let display_role_selected selected_role =
  clear_graph ();
  let message =
    Printf.sprintf "You selected: %s. Starting the game..." selected_role
  in
  let text_x = (window_width - (String.length message * 6)) / 2 in
  let text_y = window_height / 2 in
  moveto text_x text_y;
  set_color black;
  draw_string message;
  synchronize ()

(* New function to display the role completion screen *)
let display_role_complete (role, score) =
  clear_graph ();
  let message = Printf.sprintf "%s Game Over! Your Score: %d" role score in
  let text_x = (window_width - (String.length message * 6)) / 2 in
  let text_y = (window_height / 2) + 50 in
  moveto text_x text_y;
  set_color black;
  draw_string message;

  (* Button to return to role selection *)
  let button_x = (window_width - button_width) / 2 in
  let button_y = (window_height / 2) - 50 in
  draw_button button_x button_y button_width button_height "Return to Selection";

  synchronize ()

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

let handle_role_click x y =
  let roles =
    [ "Campus Police"; "Criminal Investigator"; "Traffic Cop"; "Border Patrol" ]
  in
  let total_height =
    (List.length roles * button_height) + ((List.length roles - 1) * 20)
  in
  let start_y = (window_height - total_height) / 2 in

  List.mapi
    (fun i role ->
      let button_x = (window_width - button_width) / 2 in
      let button_y = start_y + (i * (button_height + 20)) in
      (role, button_x, button_y))
    roles
  |> List.find_opt (fun (_, bx, by) ->
         x >= bx && x <= bx + button_width && y >= by && y <= by + button_height)
  |> Option.map (fun (r, _, _) -> r)

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
      else game_loop Welcome completed_roles
  | RoleSelection ->
      display_role_selection ();
      let event = wait_next_event [ Button_down ] in
      if event.button then
        match handle_role_click event.mouse_x event.mouse_y with
        | Some selected_role ->
            (* If role was already played, just ignore or allow replay *)
            game_loop (RoleSelected selected_role) completed_roles
        | None -> game_loop RoleSelection completed_roles
      else game_loop RoleSelection completed_roles
  | RoleSelected selected_role ->
      display_role_selected selected_role;
      Unix.sleepf 2.0;
      let role, score = play_role_game selected_role in
      (* After finishing the role, go to the RoleComplete state to show final
         score *)
      game_loop (RoleComplete (role, score)) ((role, score) :: completed_roles)
  | RoleComplete (role, score) ->
      display_role_complete (role, score);
      let event = wait_next_event [ Button_down ] in
      let button_x = (window_width - button_width) / 2 in
      let button_y = (window_height / 2) - 50 in
      if
        event.button && event.mouse_x >= button_x
        && event.mouse_x <= button_x + button_width
        && event.mouse_y >= button_y
        && event.mouse_y <= button_y + button_height
      then
        (* After the player sees their score for the finished role, return them
           to the role selection screen. *)
        let updated_roles = completed_roles in
        (* If all four roles are done, show final screen, else back to
           selection *)
        if List.length updated_roles = 4 then
          game_loop (FinalScreen updated_roles) updated_roles
        else game_loop RoleSelection updated_roles
      else game_loop (RoleComplete (role, score)) completed_roles
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

let initialize_gui () =
  Graphics.open_graph (Printf.sprintf " %dx%d" window_width window_height);
  Graphics.auto_synchronize false;
  Graphics.set_window_title "Police Training Game";
  game_loop Welcome []

let () = initialize_gui ()
