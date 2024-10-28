open Graphics

(* GUI Constants *)
let window_width = 600
let window_height = 400
let button_width = 200
let button_height = 50

(* Start button coordinates *)
let start_button_x = (window_width - button_width) / 2
let start_button_y = (window_height - button_height) / 2

(* Draw a button with text *)
let draw_button x y width height text =
  set_color blue;
  fill_rect x y width height;
  set_color white;
  moveto (x + 20) (y + (height / 2) - 8);
  draw_string text

(* Draw the Start button *)
let draw_start_button () =
  draw_button start_button_x start_button_y button_width button_height "Start"

(* Display the welcome message and the Start button *)
let display_welcome_message () =
  clear_graph ();
  moveto (window_width / 2 - 80) (window_height / 2 + 100);
  set_color black;
  draw_string "Welcome to the Police Training Game!";
  moveto (window_width / 2 - 90) (window_height / 2 + 80);
  draw_string "Press Start to select a role."

(* Initialize the GUI window *)
let initialize_gui () =
  open_graph (Printf.sprintf " %dx%d" window_width window_height);
  set_window_title "Police Training Game";
  display_welcome_message ();
  draw_start_button ()

(* Display role selection screen *)
let display_role_selection () =
  clear_graph ();
  moveto (window_width / 2 - 70) (window_height - 100);
  set_color black;
  draw_string "Choose Your Role:";
  draw_button (window_width / 2 - 100) (window_height / 2 + 50) button_width button_height "Campus Police";
  draw_button (window_width / 2 - 100) (window_height / 2 - 20) button_width button_height "Criminal Investigator";
  draw_button (window_width / 2 - 100) (window_height / 2 - 90) button_width button_height "Traffic Cop"

(* Display message based on chosen role *)
let display_role_message role =
  clear_graph ();
  moveto (window_width / 2 - 100) (window_height / 2);
  set_color black;
  draw_string ("You chose the role of: " ^ role)

(* Handle role selection based on mouse coordinates *)
let handle_role_selection x y =
  if y >= window_height / 2 + 50 && y <= window_height / 2 + 100 then
    display_role_message "Campus Police"
  else if y >= window_height / 2 - 20 && y <= window_height / 2 + 30 then
    display_role_message "Criminal Investigator"
  else if y >= window_height / 2 - 90 && y <= window_height / 2 - 40 then
    display_role_message "Traffic Cop"

(* Main game loop *)
let game_loop () =
  let rec loop () =
    let event = wait_next_event [Button_down] in
    if event.button then
      if event.mouse_x >= start_button_x && event.mouse_x <= start_button_x + button_width &&
         event.mouse_y >= start_button_y && event.mouse_y <= start_button_y + button_height then
        (* Start button clicked, display role selection screen *)
        (display_role_selection (); loop ())
      else
        (* Role button clicked, handle role selection *)
        (handle_role_selection event.mouse_x event.mouse_y; loop ())
    else
      loop ()
  in
  loop ()
