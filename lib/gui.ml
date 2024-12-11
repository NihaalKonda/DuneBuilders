open Graphics
open Campuspolice
open Trafficcop
open Borderpatrol
open Criminalinvestigator

(* Define the type representing the game states *)
type game_state =
  | Welcome
  | RoleSelection

(* GUI Constants *)
let window_width = 600
let window_height = 400
let button_width = 200
let button_height = 50
let current_page = ref ""

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
  current_page := "welcome";
  clear_graph ();
  let window_width = size_x () in
  let window_height = size_y () in
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
  draw_button start_button_x start_button_y button_width button_height "Start"

(* Display the role selection screen *)
let display_role_selection () =
  current_page := "role_selection";
  clear_graph ();
  let window_width = size_x () in
  let window_height = size_y () in
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
    roles

(* Determine which role is clicked based on coordinates *)
let handle_role_click x y =
  let roles =
    [ "Campus Police"; "Criminal Investigator"; "Traffic Cop"; "Border Patrol" ]
  in
  let start_y = (window_height / 2) + (70 * (List.length roles / 2)) in
  (* Each button is spaced by 70 px vertically *)
  List.mapi
    (fun i role ->
      let button_x = (window_width / 2) - 100 in
      let button_y = start_y - (i * 70) in
      (role, button_x, button_y))
    roles
  |> List.find_opt (fun (_, bx, by) ->
         x >= bx && x <= bx + button_width && y >= by && y <= by + button_height)
  |> Option.map (fun (r, _, _) -> r)

(* After selecting a role, play its corresponding game *)
(* After selecting a role, play its corresponding game and exit the GUI *)
let play_role_game role =
  close_graph ();
  (* Closes the GUI window *)
  match role with
  | "Campus Police" -> play_campus_police ()
  | "Criminal Investigator" -> play_criminal_investigator ()
  | "Traffic Cop" -> play_traffic_cop ()
  | "Border Patrol" -> play_border_patrol ()
  | _ -> ()

(* Main game loop *)
(* Main game loop *)
let rec game_loop state =
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
        then game_loop RoleSelection
        else game_loop Welcome
      else game_loop Welcome
  | RoleSelection ->
      display_role_selection ();
      let event = wait_next_event [ Button_down ] in
      if event.button then
        match handle_role_click event.mouse_x event.mouse_y with
        | Some selected_role ->
            (* Play the selected role's game and exit the GUI *)
            play_role_game selected_role
        | None -> game_loop RoleSelection
      else game_loop RoleSelection

(* Initialize the GUI window *)
let initialize_gui () =
  open_graph (Printf.sprintf " %dx%d" window_width window_height);
  set_window_title "Police Training Game";
  game_loop Welcome
