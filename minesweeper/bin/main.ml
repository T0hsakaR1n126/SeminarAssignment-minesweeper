open Minesweeper

(* 命令行参数解析 *)
let parse_args () =
  let rows = ref 8 in
  let cols = ref 8 in
  let mines = ref 10 in
  let seed = ref None in
  
  let speclist = [
    ("--rows", Arg.Set_int rows, "Number of rows (default: 8)");
    ("--cols", Arg.Set_int cols, "Number of columns (default: 8)");
    ("--mines", Arg.Set_int mines, "Number of mines (default: 10)");
    ("--seed", Arg.Int (fun s -> seed := Some s), "Random seed");
  ] in
  
  Arg.parse speclist (fun _ -> ()) "Minesweeper game";
  
  if !mines >= !rows * !cols then (
    Printf.eprintf "Error: Too many mines for the board size!\n";
    exit 1
  );
  
  (!rows, !cols, !mines, !seed)

(* 获取用户输入 *)
let get_user_input board =
  try
    print_string "Enter command (r row col to reveal, f row col to flag, q to quit): ";
    flush stdout;
    let line = read_line () in
    match String.split_on_char ' ' line with
    | ["q"] -> `Quit
    | ["r"; row_str; col_str] -> 
        let row = int_of_string row_str in
        let col = int_of_string col_str in
        if row >= 0 && row < board.rows && col >= 0 && col < board.cols then
          `Reveal (row, col)
        else
          `Invalid
    | ["f"; row_str; col_str] -> 
        let row = int_of_string row_str in
        let col = int_of_string col_str in
        if row >= 0 && row < board.rows && col >= 0 && col < board.cols then
          `Flag (row, col)
        else
          `Invalid
    | _ -> `Invalid
  with
  | Failure _ -> `Invalid
  | End_of_file -> `Quit

(* 游戏主循环 *)
let rec game_loop board =
  print_endline "\nCurrent board:";
  print_board board false;
  
  match get_user_input board with
  | `Quit -> 
      print_endline "Game quit!";
      `Quit
  | `Invalid -> 
      print_endline "Invalid input! Please try again.";
      game_loop board
  | `Reveal (row, col) ->
      let cell = board.cells.(row).(col) in
      (match cell.content with
      | Mine -> 
          print_endline "Game Over! You hit a mine!";
          print_endline "Final board:";
          print_board (reveal_all_mines board) true;
          `Lose
      | Number _ ->
          let new_board = reveal_cell board row col in
          if check_win new_board then (
            print_endline "Congratulations! You won!";
            print_endline "Final board:";
            print_board new_board true;
            `Win
          ) else
            game_loop new_board
      )
  | `Flag (row, col) ->
      let new_board = toggle_flag board row col in
      game_loop new_board

(* 主函数 *)
let () =
  let rows, cols, mines, seed = parse_args () in
  
  Printf.printf "Starting Minesweeper with %dx%d board and %d mines\n" rows cols mines;
  
  let board = init_board rows cols mines seed in
  
  match game_loop board with
  | `Win -> print_endline "Thanks for playing!"
  | `Lose -> print_endline "Better luck next time!"
  | `Quit -> print_endline "Game ended by user."