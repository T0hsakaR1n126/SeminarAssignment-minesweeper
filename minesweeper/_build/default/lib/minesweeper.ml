(* Part I. Define Cell *)
(* Define the state of the cell. *)
type cell_state = 
  | Hidden 
  | Revealed 
  | Flagged

(* Define the property(content) of the cell. *)
type cell_content = 
  | Mine 
  | Number of int

(* Define the cell. *)
type cell = {
  state : cell_state;
  content : cell_content;
  row : int;
  col : int;
}

(* Part II. Define Parameters of A Game *)
(* Define the gameboard.*)
type board = {
  cells : cell array array;
  rows : int;
  cols : int;
  mines : int;
  seed : int option;
}

(* Define the game's state. *)
type game_state = 
  | Playing 
  | Win 
  | Lose

(* Part III. Define helper functions. *)
(* Define function to create cell and initialize it. *)
let create_cell row col = {
  state = Hidden;
  content = Number 0;
  row = row;
  col = col;
}

(* Define gameboard and initialize it. *)
let init_board rows cols mines_count seed =
  (* set the seed *)
  let _ = match seed with
    | Some s -> Random.init s
    | None -> Random.self_init ()
  in
  
  (* Create cells *)
  let cells = 
    Array.init rows (fun i ->
      Array.init cols (fun j ->
        create_cell i j
      )
    )
  in
  
  (* Set mines. *)
  let placed_mines = ref 0 in
  while !placed_mines < mines_count do
    let r = Random.int rows in
    let c = Random.int cols in
    let cell = cells.(r).(c) in
    match cell.content with
    | Mine -> () 
    | Number _ -> 
      cells.(r).(c) <- { cell with content = Mine };
      incr placed_mines
  done;
  
  (* Calculate number of mines around a cell(9 x 9). *)
  let count_adjacent_mines cells row col =
    let count = ref 0 in
    for dr = -1 to 1 do
      for dc = -1 to 1 do
        if dr = 0 && dc = 0 then () else
        let nr = row + dr in
        let nc = col + dc in
        if nr >= 0 && nr < rows && nc >= 0 && nc < cols then
          match cells.(nr).(nc).content with
          | Mine -> incr count
          | Number _ -> ()
      done
    done;
    !count
  in
  
  (* Update the cell. *)
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      match cells.(i).(j).content with
      | Mine -> () 
      | Number _ -> 
        let mine_count = count_adjacent_mines cells i j in
        cells.(i).(j) <- { cells.(i).(j) with content = Number mine_count }
    done
  done;
  
  { cells; rows; cols; mines = mines_count; seed }

(* Check if coordinates are valid. *)
let is_valid_coord board row col =
  row >= 0 && row < board.rows && col >= 0 && col < board.cols

(* Reveal a cell. This is a recursive function *)
let rec reveal_cell board row col =
  if not (is_valid_coord board row col) then board
  else
    (* Find the cell and check the state of cell. Only deal with the cell whose state is hidden. The cell with this state need to be changed.*)
    let cell = board.cells.(row).(col) in
    match cell.state with
    | Revealed -> board
    | Flagged -> board
    | Hidden ->
      (* Since original cell can not be changed, a new cell need to be created and replace the original one. *)
      let new_cell = { cell with state = Revealed } in
      let new_cells = Array.copy board.cells in
      new_cells.(row) <- Array.copy board.cells.(row);
      new_cells.(row).(col) <- new_cell;
      let new_board = { board with cells = new_cells } in
      
      (* Reveal on a blank cell(Number 0) will automatically reveal all adjacent blank areas. *)
      match new_cell.content with
      | Number 0 ->
        (* From left-top corner to right-bottom corner, algorithm check the state of cell one by one. *)
        let rec reveal_all_neighbors b dr dc =
          if dr > 1 then b
          else if dc > 1 then reveal_all_neighbors b (dr + 1) (-1)
          else if dr = 0 && dc = 0 then reveal_all_neighbors b dr (dc + 1)
          else
            let nr = row + dr in
            let nc = col + dc in
            if is_valid_coord b nr nc then
              let neighbor = b.cells.(nr).(nc) in
              match neighbor.state with
              | Hidden -> 
                let updated_b = reveal_cell b nr nc in
                reveal_all_neighbors updated_b dr (dc + 1)
              | _ -> reveal_all_neighbors b dr (dc + 1)
            else
              reveal_all_neighbors b dr (dc + 1)
        in
        (* recursive start. *)
        reveal_all_neighbors new_board (-1) (-1)
      | _ -> new_board

(* Toggle flag state. *)
let toggle_flag board row col =
  if not (is_valid_coord board row col) then board
  else
    let cell = board.cells.(row).(col) in
    match cell.state with
    | Revealed -> board (* revealed cells cannot be flagged. *)
    | Hidden -> 
      let new_cell = { cell with state = Flagged } in
      let new_cells = Array.copy board.cells in
      new_cells.(row) <- Array.copy board.cells.(row);
      new_cells.(row).(col) <- new_cell;
      { board with cells = new_cells }
    | Flagged -> 
      let new_cell = { cell with state = Hidden } in
      let new_cells = Array.copy board.cells in
      new_cells.(row) <- Array.copy board.cells.(row);
      new_cells.(row).(col) <- new_cell;
      { board with cells = new_cells }

(* Define a function to check the game's state (win or lose). *)
let check_win board =
  let all_non_mine_revealed = ref true in
  for i = 0 to board.rows - 1 do
    for j = 0 to board.cols - 1 do
      let cell = board.cells.(i).(j) in
      match cell.content with
      | Mine -> () 
      | Number _ -> 
        if cell.state <> Revealed then
          all_non_mine_revealed := false
    done
  done;
  !all_non_mine_revealed

(* Define a function to reveal all mines after game ends. *)
let reveal_all_mines board =
  let new_cells = Array.init board.rows (fun i ->
    Array.init board.cols (fun j ->
      let cell = board.cells.(i).(j) in
      match cell.content with
      | Mine -> { cell with state = Revealed }
      | _ -> cell
    )
  ) in
  { board with cells = new_cells }

(* Define a function to print the gameboard. *)
let print_board board show_all =
  print_string "   ";
  for j = 0 to board.cols - 1 do
    Printf.printf "%2d " j
  done;
  print_endline "";
  
  for i = 0 to board.rows - 1 do
    Printf.printf "%2d " i;
    for j = 0 to board.cols - 1 do
      let cell = board.cells.(i).(j) in
      match cell.state with
      | Hidden when not show_all -> print_string " . "
      | Flagged when not show_all -> print_string " F "
      | _ ->
        match cell.content with
        | Mine -> print_string " * "
        | Number n -> Printf.printf "%2d " n
    done;
    print_endline ""
  done