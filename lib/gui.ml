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

let display_role_message role =
  clear_graph ();
  let message = match role with
    | "Campus Police" -> "It's 11:45 p.m. on a Friday night, and a noise complaint has come in regarding a large fraternity party happening near North Campus. Multiple students in neighboring buildings have called, expressing concerns about loud music and shouting. As the campus police officer on duty, you arrive at the fraternity house and see a large number of students congregating outside. You hear loud music coming from inside the house, and it’s clear that the party is still in full swing. What do you do?

a) Politely approach the fraternity house president, explain the noise complaint, and ask them to lower the music volume.

b) Announce to the crowd that they need to disperse and inform them that further noise complaints may result in disciplinary action.

c) Issue a formal noise violation warning to the fraternity and record the incident for university review.

d) Radio for additional campus police officers to assist in managing the situation, with a goal to close down the party entirely if compliance isn’t met."
    | "Criminal Investigator" -> "A homeowner reports a burglary that took place while they were out for the evening. Upon arrival, you find the front door slightly damaged, indicating forced entry. The homeowner informs you that several valuable items, including electronics and jewelry, are missing. You note that there are no security cameras in the home, but a few neighbors across the street may have witnessed unusual activity.

a) Interview the neighbors to gather any potential witness statements and descriptions of suspects or vehicles seen near the home.


b) Process the crime scene by dusting for fingerprints, photographing the area, and collecting evidence like broken pieces from the door or any belongings left by the intruder.

c) Review recent burglary cases in the area for any patterns, and cross-reference stolen items with local pawn shops and online marketplaces.

d) Ask the homeowner for a list of all missing items, and reassure them while advising them on preventive measures to secure their home against future incidents."
    | "Traffic Cop" -> "You're patrolling a busy intersection in a suburban area during rush hour when you observe a vehicle speeding through a red light. The driver appears to be in a hurry, and the traffic behind them brakes suddenly to avoid a collision. You pull the vehicle over and approach the driver, who seems visibly anxious.

a) Ask the driver to provide their license and registration, explain the danger of running a red light, and issue a warning instead of a ticket.

b) Politely but firmly explain the violation, ask for license and registration, and issue a traffic citation.

c) Ask the driver if there is an emergency and try to understand why they ran the red light before deciding on a course of action.

d) Request license and registration, check for any outstanding violations, and issue both a ticket and a mandatory safety class citation."
    | _ -> "Unknown role"
  in
  moveto (window_width / 2 - 100) (window_height / 2);
  set_color black;
  draw_string message
  

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
