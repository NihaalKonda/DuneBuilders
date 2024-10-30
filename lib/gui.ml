open Graphics

(* GUI Constants *)
let window_width = 600
let window_height = 400
let button_width = 200
let button_height = 50

(* Start button coordinates *)
let start_button_x = (window_width - button_width) / 2
let start_button_y = (window_height - button_height) / 2

(* Page tracking variable *)
let current_page = ref "welcome"

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
  current_page := "welcome";
  clear_graph ();
  moveto (window_width / 2 - 80) (window_height / 2 + 100);
  set_color black;
  draw_string "Welcome to the Police Training Game!";
  moveto (window_width / 2 - 90) (window_height / 2 + 80);
  draw_string "Press Start to select a role.";
  draw_start_button ()

(* Display role selection screen *)
let display_role_selection () =
  current_page := "role_selection";
  clear_graph ();
  moveto (window_width / 2 - 70) (window_height - 100);
  set_color black;
  draw_string "Choose Your Role:";
  draw_button (window_width / 2 - 100) (window_height / 2 + 50) button_width button_height "Campus Police";
  draw_button (window_width / 2 - 100) (window_height / 2 - 20) button_width button_height "Criminal Investigator";
  draw_button (window_width / 2 - 100) (window_height / 2 - 90) button_width button_height "Traffic Cop"

(* Display role-specific message *)
let display_role_message role =
  clear_graph ();
  let message = match role with
    | "Campus Police" -> 
        "It's 11:45 p.m. on a Friday night, and a noise complaint has come in regarding a large fraternity party happening near North Campus. Multiple students in neighboring buildings have called, expressing concerns about loud music and shouting. As the campus police officer on duty, you arrive at the fraternity house and see a large number of students congregating outside. You hear loud music coming from inside the house, and it’s clear that the party is still in full swing. What do you do?\n\
        \n\
        a) Politely approach the fraternity house president, explain the noise complaint, and ask them to lower the music volume.\n\
        b) Announce to the crowd that they need to disperse and inform them that further noise complaints may result in disciplinary action.\n\
        c) Issue a formal noise violation warning to the fraternity and record the incident for university review.\n\
        d) Radio for additional campus police officers to assist in managing the situation, with a goal to close down the party entirely if compliance isn’t met."
    | "Criminal Investigator" -> 
        "A homeowner reports a burglary that took place while they were out for the evening. Upon arrival, you find the front door slightly damaged, indicating forced entry. The homeowner informs you that several valuable items, including electronics and jewelry, are missing. You note that there are no security cameras in the home, but a few neighbors across the street may have witnessed unusual activity.\n\
        \n\
        a) Interview the neighbors to gather any potential witness statements and descriptions of suspects or vehicles seen near the home.\n\
        b) Process the crime scene by dusting for fingerprints, photographing the area, and collecting evidence like broken pieces from the door or any belongings left by the intruder.\n\
        c) Review recent burglary cases in the area for any patterns, and cross-reference stolen items with local pawn shops and online marketplaces.\n\
        d) Ask the homeowner for a list of all missing items, and reassure them while advising them on preventive measures to secure their home against future incidents."
    | "Traffic Cop" -> 
        "You're patrolling a busy intersection in a suburban area during rush hour when you observe a vehicle speeding through a red light. The driver appears to be in a hurry, and the traffic behind them brakes suddenly to avoid a collision. You pull the vehicle over and approach the driver, who seems visibly anxious.\n\
        \n\
        a) Ask the driver to provide their license and registration, explain the danger of running a red light, and issue a warning instead of a ticket.\n\
        b) Politely but firmly explain the violation, ask for license and registration, and issue a traffic citation.\n\
        c) Ask the driver if there is an emergency and try to understand why they ran the red light before deciding on a course of action.\n\
        d) Request license and registration, check for any outstanding violations, and issue both a ticket and a mandatory safety class citation."
    | _ -> "Unknown role"
  in
  
  (* Function to split text into lines based on the window width *)
  let format_text text max_pixel_width =
    let words = String.split_on_char ' ' text in
    let rec aux current_line current_width lines = function
      | [] -> List.rev (String.concat " " (List.rev current_line) :: lines)
      | word :: rest ->
        let word_width = (String.length word + 1) * 6 in (* rough width in pixels per character *)
        if current_width + word_width > max_pixel_width then
          aux [word] word_width (String.concat " " (List.rev current_line) :: lines) rest
        else
          aux (word :: current_line) (current_width + word_width) lines rest
    in
    aux [] 0 [] words
  in

  (* Define the area width based on screen size *)
  let max_text_width = window_width - 80 in
  let lines = format_text message max_text_width in
  let line_height = 20 in

  (* Calculate starting y-position to center the text block *)
  let total_text_height = List.length lines * line_height in
  let y_start = (window_height / 2) + (total_text_height / 2) in

  set_color black;
  List.iteri (fun i line ->
    let line_width = String.length line * 6 in
    let x_start = (window_width - line_width) / 2 in (* Center each line *)
    moveto x_start (y_start - i * line_height);
    draw_string line
  ) lines

let handle_role_selection x y =
  if x >= window_width / 2 - 100 && x <= window_width / 2 - 100 + button_width then
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
      match !current_page with
      | "welcome" ->
        if event.mouse_x >= start_button_x && event.mouse_x <= start_button_x + button_width &&
           event.mouse_y >= start_button_y && event.mouse_y <= start_button_y + button_height then
          (* Start button clicked, display role selection screen *)
          (display_role_selection (); loop ())
        else
          loop ()
      | "role_selection" ->
        (* Role button clicked, handle role selection *)
        handle_role_selection event.mouse_x event.mouse_y;
        loop ()
      | _ -> loop ()
  in
  loop ()

(* Initialize the GUI window *)
let initialize_gui () =
  open_graph (Printf.sprintf " %dx%d" window_width window_height);
  set_window_title "Police Training Game";
  display_welcome_message ();
  game_loop ()

(* Run the GUI application *)
let () = initialize_gui ()
