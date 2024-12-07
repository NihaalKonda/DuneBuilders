open Graphics
open Role

(* Define the type representing the game states *)
type game_state = Welcome | RoleSelection | Scenario of Role.scenario

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
  let text_x = x + (width - (String.length text * 6)) / 2 in
  let text_y = y + (height / 2) - 8 in
  moveto text_x text_y;
  draw_string text

(* Display the welcome screen *)
let display_welcome_message () =
  current_page := "welcome";
  clear_graph ();
  moveto (window_width / 2 - 100) (window_height / 2 + 100);
  set_color black;
  draw_string "Welcome to the Police Training Game!";
  let start_button_x = (window_width - button_width) / 2 in
  let start_button_y = (window_height - button_height) / 2 in
  draw_button start_button_x start_button_y button_width button_height "Start"

(* Display the role selection screen *)
let display_role_selection () =
  current_page := "role_selection";
  clear_graph ();
  moveto (window_width / 2 - 70) (window_height - 100);
  set_color black;
  draw_string "Choose Your Role:";
  let roles = ["Campus Police"; "Criminal Investigator"; "Traffic Cop"] in
  List.iteri (fun i role ->
    let y = (window_height / 2) + 50 - (i * 70) in
    draw_button (window_width / 2 - 100) y button_width button_height role
  ) roles

(* Display a scenario screen *)
let display_scenario scenario =
  clear_graph ();
  set_color black;

  (* Display title *)
  moveto (window_width / 2 - (String.length scenario.title * 3)) (window_height - 50);
  draw_string scenario.title;

  (* Display description *)
  let lines = String.split_on_char '\n' scenario.description in
  List.iteri (fun i line ->
    moveto 50 (window_height - 100 - (i * 20));
    draw_string line
  ) lines;

  (* Display options *)
  List.iteri (fun i option ->
    let y = 200 - i * 40 in
    draw_button 100 y 400 30 option
  ) scenario.options

(* Main game loop *)
let rec game_loop state =
  match state with
  | Welcome ->
      display_welcome_message ();
      let event = wait_next_event [Button_down] in
      if event.button then
        let start_button_x = (window_width - button_width) / 2 in
        let start_button_y = (window_height - button_height) / 2 in
        if event.mouse_x >= start_button_x && event.mouse_x <= start_button_x + button_width &&
           event.mouse_y >= start_button_y && event.mouse_y <= start_button_y + button_height then
          game_loop RoleSelection
        else
          game_loop Welcome
      else
        game_loop Welcome

        | RoleSelection ->
          display_role_selection ();
          let event = wait_next_event [Button_down] in
          if event.button then
            let chosen_role = Role.handle_role_selection event.mouse_x event.mouse_y in
            match chosen_role with
            | Some selected_role ->
                close_graph (); (* Exit GUI mode *)
                (* Now switch to terminal mode *)
                handle_scenarios_terminal selected_role
            | None -> game_loop RoleSelection
          else
            game_loop RoleSelection
      

  | Scenario scenario ->
      display_scenario scenario;
      let event = wait_next_event [Button_down] in
      if event.button then
        game_loop Welcome
      else
        game_loop (Scenario scenario)

(* Initialize the GUI window *)
let initialize_gui () =
  open_graph (Printf.sprintf " %dx%d" window_width window_height);
  set_window_title "Police Training Game";
  display_welcome_message ()