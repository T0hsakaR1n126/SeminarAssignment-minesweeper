open Minesweeper

(* Part I. User Command Parsing From Command Line *)  
let parse_args () =
  let rows = ref 8 in
  let cols = ref 8 in
  let mines = ref 10 in
  let seed = ref None in

  (* Use the parameters entered via the command line to set the configuration for this Minesweeper game. *)
  let speclist = [
    ("--rows", Arg.Set_int rows, "Number of rows (default: 8)");
    ("--cols", Arg.Set_int cols, "Number of columns (default: 8)");
    ("--mines", Arg.Set_int mines, "Number of mines (default: 10)");
    ("--seed", Arg.Int (fun s -> seed := Some s), "Random seed");
  ] in
  
  Arg.parse speclist (fun _ -> ()) "Minesweeper game";
  
  (* Check if the number of mines exceeds the size of the chessboard. The game is illegal if there are too many mines. *)
  if !mines >= !rows * !cols then (
    Printf.eprintf "Error: Too many mines for the board size!\n";
    exit 1
  );
  
  (* Return the parsed parameter tuple *)
  (!rows, !cols, !mines, !seed)

(* Part II. Get User Input During Gameplay *)
let get_user_input board =
  try
    print_string "Enter command (r row col to reveal, f row col to flag(type again to cancel flagging), q to quit): ";
    flush stdout; (* This code is used to force a flush to the output buffer to ensure the prompt appears immediately. *)
    
    let line = read_line () in
    match String.split_on_char ' ' line with
    
    (* Deal with input of form 'q'. *)
    | ["q"] -> `Quit
    
    (* Deal with input of form 'r row col'. *)
    | ["r"; row_str; col_str] -> 
      let row = int_of_string row_str in
      let col = int_of_string col_str in
      if row >= 0 && row < board.rows && col >= 0 && col < board.cols then
        `Reveal (row, col)
      else
        `Invalid
    
    (* Deal with input of form 'f row col'. *)
    | ["f"; row_str; col_str] -> 
      let row = int_of_string row_str in
      let col = int_of_string col_str in
      if row >= 0 && row < board.rows && col >= 0 && col < board.cols then
        `Flag (row, col)
      else
        `Invalid
    
    (* All other forms of input are invalid. *)
    | _ -> `Invalid
  
  with
  (* Exception handling *)
  | Failure _ -> `Invalid
  | End_of_file -> `Quit

(* Part III. Game Main Loop *)
let rec game_loop board =
  (* Print the current chessboard state *)
  print_endline "\nCurrent board:";
  print_board board false;
  
  match get_user_input board with
  | `Quit -> 
    print_endline "Game quit!";
    `Quit
  
  | `Invalid -> 
    print_endline "Invalid input! Please try again.";
    game_loop board
  
  (* Handling the revealing operation of user and check game's state. *)
  | `Reveal (row, col) ->
    let cell = board.cells.(row).(col) in
    let result = match cell.content with
    | Mine -> 
      print_endline "Game Over! You hit a mine!";
      print_endline "Final board:";
      print_board (reveal_all_mines board) true;
      `Lose
    | Number _ ->
      let new_board = reveal_cell board row col in
      if check_win new_board then (
        print_endline "Congratulations! You sweep all mines!";
        print_endline "Final board:";
        print_board new_board true;
        `Win
      ) else
        game_loop new_board
    in
    result
    
  | `Flag (row, col) ->
    let new_board = toggle_flag board row col in
    game_loop new_board

(* Part IV. Main function *)
let () =
  let rows, cols, mines, seed = parse_args () in
  
  Printf.printf "Starting Minesweeper with %dx%d board and %d mines\n" rows cols mines;
  
  let board = init_board rows cols mines seed in
  
  match game_loop board with
  | `Win -> print_endline "Thank you for playing!"
  | `Lose -> print_endline "Better luck next time."
  | `Quit -> print_endline "Game ended by user."