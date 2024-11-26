open Graphics

(* Type to represent roles *)
type role = CampusPolice | CriminalInvestigator | TrafficCop

(* Handle role selection based on mouse coordinates *)
let handle_role_selection x y =
  if x >= (600 / 2 - 100) && x <= (600 / 2 + 100) then
    if y >= (400 / 2 + 50) && y <= (400 / 2 + 100) then Some CampusPolice
    else if y >= (400 / 2 - 20) && y <= (400 / 2 + 30) then Some CriminalInvestigator
    else if y >= (400 / 2 - 90) && y <= (400 / 2 - 40) then Some TrafficCop
    else None
  else
    None

(* Display the message for a selected role *)
let display_role_message role =
  clear_graph ();
  let message = match role with
    | CampusPolice -> 
        "Campus Police Scenario:\n
        It's 11:45 p.m. on a Friday night. Noise complaint near North Campus..."
    | CriminalInvestigator -> 
        "Criminal Investigator Scenario:\n
        A burglary has been reported. Front door forced open..."
    | TrafficCop -> 
        "Traffic Cop Scenario:\n
        A vehicle ran a red light. The driver appears anxious..."
  in

  (* Function to split text into lines *)
  let format_text text max_pixel_width =
    let words = String.split_on_char ' ' text in
    let rec aux current_line current_width lines = function
      | [] -> List.rev (String.concat " " (List.rev current_line) :: lines)
      | word :: rest ->
        let word_width = (String.length word + 1) * 6 in
        if current_width + word_width > max_pixel_width then
          aux [word] word_width (String.concat " " (List.rev current_line) :: lines) rest
        else
          aux (word :: current_line) (current_width + word_width) lines rest
    in
    aux [] 0 [] words
  in

  (* Format and display the message *)
  let max_text_width = 600 - 80 in
  let lines = format_text message max_text_width in
  let line_height = 20 in
  let total_text_height = List.length lines * line_height in
  let y_start = (400 / 2) + (total_text_height / 2) in
  set_color black;
  List.iteri (fun i line ->
    let line_width = String.length line * 6 in
    let x_start = (600 - line_width) / 2 in
    moveto x_start (y_start - i * line_height);
    draw_string line
  ) lines
