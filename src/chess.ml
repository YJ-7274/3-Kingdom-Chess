open Raylib
open Board
open Pieces

let existing_kings = ref [ 1; 1; 1 ]
let existing_kings_one_round_ago = ref !existing_kings
let existing_kings_two_rounds_ago = ref !existing_kings

(*returns [Some color] if that color king is the only one left. Otherwise,
  return None*)
let check_king_winner (kings : int list ref) =
  match !kings with
  | [ 1; 0; 0 ] -> Some Red
  | [ 0; 1; 0 ] -> Some Yellow
  | [ 0; 0; 1 ] -> Some Orange
  | _ -> None

let still_in_game (kings : int list ref) (color : Pieces.color) =
  match color with
  | Red -> begin
      match !kings with
      | [ 1; _; _ ] -> true
      | _ -> false
    end
  | Yellow -> begin
      match !kings with
      | [ _; 1; _ ] -> true
      | _ -> false
    end
  | Orange -> begin
      match !kings with
      | [ _; _; 1 ] -> true
      | _ -> false
    end

let convert_colors (winner_color : Pieces.color) (loser_color : Pieces.color)
    (pieces : Pieces.piece array array) =
  begin
    Array.iter
      (fun piece_array ->
        Array.iter
          (fun piece ->
            if piece.color = loser_color then piece.color <- winner_color)
          piece_array)
      pieces
  end

let eliminate_king_and_take_over (winner_color : Pieces.color)
    (loser_color : Pieces.color) (pieces : Pieces.piece array array) =
  convert_colors winner_color loser_color pieces;
  begin
    match loser_color with
    | Red -> begin
        match !existing_kings with
        | [ _; s; t ] -> existing_kings := [ 0; s; t ]
        | _ -> failwith "List does not have exactly three elements"
      end
    | Yellow -> begin
        match !existing_kings with
        | [ f; _; t ] -> existing_kings := [ f; 0; t ]
        | _ -> failwith "List does not have exactly three elements"
      end
    | Orange -> begin
        match !existing_kings with
        | [ f; s; _ ] -> existing_kings := [ f; s; 0 ]
        | _ -> failwith "List does not have exactly three elements"
      end
  end

let red_pawns =
  ref
    (Array.init 8 (fun i ->
         let file = Char.chr (Char.code 'A' + i) in
         let pawn = Pawn.create Red in
         pawn.position <- BoardFunctions.board_center_points file 2;
         pawn))

let red_rooks =
  ref
    [|
      (let rook = Rook.create Red in
       rook.position <- BoardFunctions.board_center_points 'A' 1;
       rook);
      (let rook = Rook.create Red in
       rook.position <- BoardFunctions.board_center_points 'H' 1;
       rook);
    |]

let red_knights =
  ref
    [|
      (let knight = Knight.create Red in
       knight.position <- BoardFunctions.board_center_points 'B' 1;
       knight);
      (let knight = Knight.create Red in
       knight.position <- BoardFunctions.board_center_points 'G' 1;
       knight);
    |]

let red_bishops =
  ref
    [|
      (let bishop = Bishop.create Red in
       bishop.position <- BoardFunctions.board_center_points 'C' 1;
       bishop);
      (let bishop = Bishop.create Red in
       bishop.position <- BoardFunctions.board_center_points 'F' 1;
       bishop);
    |]

let red_queen =
  ref
    [|
      (let queen = Queen.create Red in
       queen.position <- BoardFunctions.board_center_points 'D' 1;
       queen);
    |]

let red_king =
  ref
    [|
      (let king = King.create Red in
       king.position <- BoardFunctions.board_center_points 'E' 1;
       king);
    |]

let yellow_pawns =
  ref
    (Array.concat
       [
         Array.init 4 (fun i ->
             let file = Char.chr (Char.code 'A' + i) in
             let pawn = Pawn.create Yellow in
             pawn.position <- BoardFunctions.board_center_points file 7;
             pawn);
         Array.init 4 (fun i ->
             let file = Char.chr (Char.code 'I' + i) in
             let pawn = Pawn.create Yellow in
             pawn.position <- BoardFunctions.board_center_points file 7;
             pawn);
       ])

let yellow_rooks =
  ref
    [|
      (let rook = Rook.create Yellow in
       rook.position <- BoardFunctions.board_center_points 'A' 8;
       rook);
      (let rook = Rook.create Yellow in
       rook.position <- BoardFunctions.board_center_points 'L' 8;
       rook);
    |]

let yellow_knights =
  ref
    [|
      (let knight = Knight.create Yellow in
       knight.position <- BoardFunctions.board_center_points 'B' 8;
       knight);
      (let knight = Knight.create Yellow in
       knight.position <- BoardFunctions.board_center_points 'K' 8;
       knight);
    |]

let yellow_bishops =
  ref
    [|
      (let bishop = Bishop.create Yellow in
       bishop.position <- BoardFunctions.board_center_points 'C' 8;
       bishop);
      (let bishop = Bishop.create Yellow in
       bishop.position <- BoardFunctions.board_center_points 'J' 8;
       bishop);
    |]

let yellow_queen =
  ref
    [|
      (let queen = Queen.create Yellow in
       queen.position <- BoardFunctions.board_center_points 'I' 8;
       queen);
    |]

let yellow_king =
  ref
    [|
      (let king = King.create Yellow in
       king.position <- BoardFunctions.board_center_points 'D' 8;
       king);
    |]

let orange_pawns =
  ref
    (Array.concat
       [
         Array.init 4 (fun i ->
             let file = Char.chr (Char.code 'E' + i) in
             let pawn = Pawn.create Orange in
             pawn.position <- BoardFunctions.board_center_points file 11;
             pawn);
         Array.init 4 (fun i ->
             let file = Char.chr (Char.code 'I' + i) in
             let pawn = Pawn.create Orange in
             pawn.position <- BoardFunctions.board_center_points file 11;
             pawn);
       ])

let orange_rooks =
  ref
    [|
      (let rook = Rook.create Orange in
       rook.position <- BoardFunctions.board_center_points 'H' 12;
       rook);
      (let rook = Rook.create Orange in
       rook.position <- BoardFunctions.board_center_points 'L' 12;
       rook);
    |]

let orange_knights =
  ref
    [|
      (let knight = Knight.create Orange in
       knight.position <- BoardFunctions.board_center_points 'G' 12;
       knight);
      (let knight = Knight.create Orange in
       knight.position <- BoardFunctions.board_center_points 'K' 12;
       knight);
    |]

let orange_bishops =
  ref
    [|
      (let bishop = Bishop.create Orange in
       bishop.position <- BoardFunctions.board_center_points 'F' 12;
       bishop);
      (let bishop = Bishop.create Orange in
       bishop.position <- BoardFunctions.board_center_points 'J' 12;
       bishop);
    |]

let orange_queen =
  ref
    [|
      (let queen = Queen.create Orange in
       queen.position <- BoardFunctions.board_center_points 'E' 12;
       queen);
    |]

let orange_king =
  ref
    [|
      (let king = King.create Orange in
       king.position <- BoardFunctions.board_center_points 'I' 12;
       king);
    |]

let all_pieces =
  ref
    [|
      !red_pawns;
      !red_rooks;
      !red_knights;
      !red_bishops;
      !red_queen;
      !red_king;
      !yellow_pawns;
      !yellow_rooks;
      !yellow_knights;
      !yellow_bishops;
      !yellow_queen;
      !yellow_king;
      !orange_pawns;
      !orange_rooks;
      !orange_knights;
      !orange_bishops;
      !orange_queen;
      !orange_king;
    |]

let color_to_string color =
  match color with
  | Red -> "Red"
  | Yellow -> "Yellow"
  | Orange -> "Orange"

let kind_to_string kind =
  match kind with
  | Pawn -> "Pawn"
  | Rook -> "Rook"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Queen -> "Queen"
  | King -> "King"

let copy_piece p =
  {
    id = p.id;
    kind = p.kind;
    color = p.color;
    position = p.position;
    picked = false;
    has_moved = p.has_moved;
  }

let messages1 = [ "Press 'S' to start"; "Press 'Q' to quit" ]
let messages2 = [ "Press 'Q' to quit"; "Press 'R' to replay" ]
let line_height = 24
let padding = 12
let x_position = 10
let wei_color = Color.create 180 0 0 255
let shu_color = Color.create 255 100 0 255
let wu_color = Color.create 204 195 0 255

let deep_copy_all_pieces all_pieces =
  Array.map (fun piece_array -> Array.map copy_piece piece_array) all_pieces

let initial_setup = deep_copy_all_pieces !all_pieces
let two_rounds_ago_setup = ref (deep_copy_all_pieces !all_pieces)
let one_rounds_ago_setup = ref (deep_copy_all_pieces !all_pieces)
let is_piece_picked = ref false
let turn_counter = ref 0

let current_turn () =
  if
    still_in_game existing_kings Red
    && still_in_game existing_kings Orange
    && still_in_game existing_kings Yellow
  then
    (* All three players are in the game *)
    match !turn_counter mod 3 with
    | 0 -> Red
    | 1 -> Orange
    | 2 -> Yellow
    | _ -> failwith "Unexpected turn value"
  else if
    still_in_game existing_kings Red && still_in_game existing_kings Orange
  then
    (* Only Red and Orange are in the game *)
    match !turn_counter mod 2 with
    | 0 -> Red
    | 1 -> Orange
    | _ -> failwith "Unexpected turn value"
  else if
    still_in_game existing_kings Red && still_in_game existing_kings Yellow
  then
    (* Only Red and Yellow are in the game *)
    match !turn_counter mod 2 with
    | 0 -> Red
    | 1 -> Yellow
    | _ -> failwith "Unexpected turn value"
  else if
    still_in_game existing_kings Orange && still_in_game existing_kings Yellow
  then
    (* Only Orange and Yellow are in the game *)
    match !turn_counter mod 2 with
    | 0 -> Orange
    | 1 -> Yellow
    | _ -> failwith "Unexpected turn value"
  else if still_in_game existing_kings Red then Red
  else if still_in_game existing_kings Orange then Orange
  else if still_in_game existing_kings Yellow then Yellow
  else failwith "Unexpected game state"

let running = ref true
let game_started = ref false
let game_over = ref false
let winning_side = ref None
let can_report = ref true
let can_report_wei = ref true
let can_report_shu = ref true
let can_report_wu = ref true

let reset_game () =
  (* Reset the game state to initial conditions *)
  game_over := false;
  winning_side := None;
  turn_counter := 0;
  game_started := false;
  all_pieces := deep_copy_all_pieces initial_setup

let tuple_to_string (a, b) = Printf.sprintf "(%c, %s)" a (string_of_int b)

(** Function to check if a square contains a piece of the same color *)
let square_contains_same_color_piece pieces color square =
  Array.exists
    (fun piece_array ->
      Array.exists
        (fun piece ->
          piece.color = color
          && piece.position
             = BoardFunctions.board_center_points (fst square) (snd square))
        piece_array)
    pieces

(** [remove_piece_at_position pieces position] returns a new [pieces] array
    without the piece at the given position; Function to remove a piece from the
    array if it's at a given position *)
let remove_piece_at_position pieces position =
  Array.map
    (fun piece_array ->
      Array.of_list
        (List.filter
           (fun piece -> piece.position <> position)
           (Array.to_list piece_array)))
    pieces

(** Function to check if a square contains an opponent's piece *)
let square_contains_opponent_piece pieces color square =
  Array.exists
    (fun piece_array ->
      Array.exists
        (fun piece ->
          piece.color <> color
          && piece.position
             = BoardFunctions.board_center_points (fst square) (snd square))
        piece_array)
    pieces

(** Function to check if a square contains any piece *)
let square_contains_piece pieces square =
  Array.exists
    (fun piece_array ->
      Array.exists
        (fun piece ->
          piece.position
          = BoardFunctions.board_center_points (fst square) (snd square))
        piece_array)
    pieces

(* Function to check if a square is empty *)
let is_square_empty pieces square = not (square_contains_piece pieces square)

let squares_between_empty pieces king_pos rook_pos =
  let empty_squares_for_castling =
    match (king_pos, rook_pos) with
    | ('E', 1), ('A', 1) ->
        [ ('B', 1); ('C', 1); ('D', 1) ] (* Red: Queen side *)
    | ('E', 1), ('H', 1) -> [ ('F', 1); ('G', 1) ] (* Red: King side *)
    | ('I', 12), ('L', 12) -> [ ('J', 12); ('K', 12) ] (* Orange: King side *)
    | ('I', 12), ('H', 12) ->
        [ ('E', 12); ('F', 12); ('G', 12) ] (* Orange: Queen side *)
    | ('D', 8), ('A', 8) -> [ ('B', 8); ('C', 8) ] (* Yellow: King side *)
    | ('D', 8), ('L', 8) ->
        [ ('I', 8); ('J', 8); ('K', 8) ] (* Yellow: Queen side *)
    | _ -> [] (* No castling path for other positions *)
  in
  (*Printf.printf "Checking squares between king at %c%d and rook at %c%d:\n"
    (fst king_pos) (snd king_pos) (fst rook_pos) (snd rook_pos);*)
  List.for_all
    (fun square ->
      let square_empty = is_square_empty pieces square in
      (*Printf.printf "Square %c%d is %s\n" (fst square) (snd square) (if
        square_empty then "empty" else "occupied");*)
      square_empty)
    empty_squares_for_castling

(* Function to check if a piece has not moved and is at a specific position *)
let has_not_moved_and_at_position piece expected_kind expected_position =
  (not piece.has_moved) && piece.kind = expected_kind
  && begin
       match
         BoardFunctions.mouse_position_to_square (fst piece.position)
           (snd piece.position)
       with
       | Some pos -> pos = expected_position
       | None -> false
     end

(* Function to check if castling is possible *)
let can_castle king rook all_pieces =
  if king.has_moved || rook.has_moved then false
  else
    let king_pos, rook_pos =
      match king.color with
      | Red ->
          if rook.position = BoardFunctions.board_center_points 'H' 1 then
            (('E', 1), ('H', 1)) (* Red: King side *)
          else (('E', 1), ('A', 1)) (* Red: Queen side *)
      | Orange ->
          if rook.position = BoardFunctions.board_center_points 'L' 12 then
            (('I', 12), ('L', 12)) (* Orange: King side *)
          else (('I', 12), ('H', 12)) (* Orange: Queen side *)
      | Yellow ->
          if rook.position = BoardFunctions.board_center_points 'A' 8 then
            (('D', 8), ('A', 8)) (* Yellow: King side *)
          else (('D', 8), ('L', 8))
      (* Yellow: Queen side *)
    in
    squares_between_empty all_pieces king_pos rook_pos

let handle_castling_attempt king rook all_pieces =
  match
    ( BoardFunctions.mouse_position_to_square (fst king.position)
        (snd king.position),
      BoardFunctions.mouse_position_to_square (fst rook.position)
        (snd rook.position) )
  with
  | Some king_square, Some rook_square when can_castle king rook !all_pieces ->
      let new_king_pos, new_rook_pos =
        match (king.color, king_square, rook_square) with
        | Red, ('E', 1), ('A', 1) ->
            (('C', 1), ('D', 1)) (* Red: Queen side castling *)
        | Red, ('E', 1), ('H', 1) ->
            (('G', 1), ('F', 1)) (* Red: King side castling *)
        | Orange, ('I', 12), ('H', 12) ->
            (('F', 12), ('E', 12)) (* Orange: Queen side castling *)
        | Orange, ('I', 12), ('L', 12) ->
            (('K', 12), ('J', 12)) (* Orange: King side castling *)
        | Yellow, ('D', 8), ('A', 8) ->
            (('B', 8), ('C', 8)) (* Yellow: King side castling *)
        | Yellow, ('D', 8), ('L', 8) ->
            (('J', 8), ('I', 8)) (* Yellow: Queen side castling *)
        | _ -> failwith "Invalid castling configuration"
      in
      let x_king, y_king =
        BoardFunctions.board_center_points (fst new_king_pos) (snd new_king_pos)
      in
      let x_rook, y_rook =
        BoardFunctions.board_center_points (fst new_rook_pos) (snd new_rook_pos)
      in
      king.position <- (x_king, y_king);
      rook.position <- (x_rook, y_rook);
      king.has_moved <- true;
      rook.has_moved <- true;
      true (* Castling was successful *)
  | _ -> false (* Castling not possible or squares are not valid *)

(* Function to get new positions for the king and rook after castling *)
let get_castling_new_positions color king_pos =
  match (color, king_pos) with
  | Red, ('G', 1) -> (('G', 1), ('F', 1)) (* Red: King side castling *)
  | Red, ('C', 1) -> (('C', 1), ('D', 1)) (* Red: Queen side castling *)
  | Orange, ('K', 12) -> (('K', 12), ('J', 12)) (* Orange: King side castling *)
  | Orange, ('F', 12) ->
      (('F', 12), ('E', 12)) (* Orange: Queen side castling *)
  | Yellow, ('B', 8) -> (('B', 8), ('C', 8)) (* Yellow: King side castling *)
  | Yellow, ('J', 8) -> (('J', 8), ('I', 8)) (* Yellow: Queen side castling *)
  | _ -> failwith "Invalid castling configuration"

(** [find_captured_piece pieces position] returns [Some piece] found at the
    given position, if no piece found return [None]; Function to find a piece at
    a given position *)
let find_captured_piece pieces position =
  let found_piece = ref None in
  Array.iter
    (fun piece_array ->
      Array.iter
        (fun piece ->
          if piece.position = position then found_piece := Some piece)
        piece_array)
    pieces;
  !found_piece

let square_contains_color_king pieces color square =
  Array.exists
    (fun piece_array ->
      Array.exists
        (fun piece ->
          piece.color = color && piece.kind = King
          && piece.position
             = BoardFunctions.board_center_points (fst square) (snd square))
        piece_array)
    pieces

(** [occupied_1_of_win_squares pieces squares color] return [true] if any of
    [squares] has a King pieces with the given [color] *)
let occupied_1_of_win_squares (pieces : Pieces.piece array array)
    (squares : (char * int) array) (color : Pieces.color) : bool =
  (* let red = Color.create 180 0 0 255 in let orange = Color.create 255 100 0
     255 in let yellow = Color.create 204 195 0 255 in *)
  let square_contains_red acc square =
    acc || square_contains_color_king pieces Red square
  in
  let square_contains_orange acc square =
    acc || square_contains_color_king pieces Orange square
  in
  let square_contains_yellow acc square =
    acc || square_contains_color_king pieces Yellow square
  in
  match color with
  | Red -> Array.fold_left square_contains_red false squares
  | Orange -> Array.fold_left square_contains_orange false squares
  | Yellow -> Array.fold_left square_contains_yellow false squares

let () =
  let screen_width = 1280 in
  let screen_height = 720 in

  ignore (init_window screen_width screen_height "Chessboard Display");

  let board_texture = load_texture "chessboard.png" in

  let board_width = 1280 in
  (* Specified dimensions of the chessboard.png texture *)
  let board_height = 720 in

  (* Specified dimensions of the chessboard.png texture *)
  set_target_fps 60;

  let display_string = ref "" in
  let position_str = ref "" in
  let report = ref "" in
  (* String to display the pick and drop positions *)
  while !running && not (window_should_close ()) do
    begin
      if !game_started = false && not !game_over then
        if is_key_pressed Key.S then game_started := true;
      if is_key_pressed Key.Q then running := false;
      if !game_over then if is_key_pressed Key.R then reset_game ();
      begin_drawing ();

      clear_background (Color.create 245 245 245 255);

      let draw_pos_x = (screen_width - board_width) / 2 in
      let draw_pos_y = (screen_height - board_height) / 2 in

      let dest_rect =
        Rectangle.create (float_of_int draw_pos_x) (float_of_int draw_pos_y)
          (float_of_int board_width)
          (float_of_int board_height)
      in
      let origin = Vector2.create 0.0 0.0 in
      draw_texture_pro board_texture
        (Rectangle.create 0.0 0.0 (float_of_int board_width)
           (float_of_int board_height))
        dest_rect origin 0.0
        (Color.create 255 255 255 255);

      (* Check if the game is over and display the winner if it is *)
      if !game_over then begin
        let winner_string, winner_color =
          match !winning_side with
          | Some Red -> ("Wei Wins! Game Over!", wei_color)
          | Some Orange -> ("Shu Wins! Game Over!", shu_color)
          | Some Yellow -> ("Wu Wins! Game Over!", wu_color)
          | None -> failwith "impossible"
        in
        draw_text winner_string 10 60 20 winner_color
      end;

      let mouse_position = get_mouse_position () in
      let adjusted_x =
        Vector2.x mouse_position -. float_of_int (screen_width / 2)
      in
      let adjusted_y =
        Vector2.y mouse_position -. float_of_int (screen_height / 2)
      in

      if (not !game_over) && !game_started then begin
        if is_mouse_button_released MouseButton.Left then begin
          position_str :=
            Printf.sprintf "(%s, %s)"
              (string_of_float adjusted_x)
              (string_of_float adjusted_y);
          match
            BoardFunctions.mouse_position_to_square adjusted_x adjusted_y
          with
          | Some (c, i) -> display_string := tuple_to_string (c, i)
          | None -> display_string := "None"
        end;

        if is_mouse_button_released MouseButton.Left then begin
          is_piece_picked := false;
          match
            BoardFunctions.mouse_position_to_square adjusted_x adjusted_y
          with
          | Some (c, i) ->
              if c = 'R' && !turn_counter > 0 then
                if current_turn () = Red && !can_report then
                  if i = 2 && !can_report_shu && !turn_counter > 1 then (
                    report := "report_shu";
                    can_report_shu := false;
                    can_report_wu := false;
                    can_report := false;
                    decr turn_counter;
                    decr turn_counter;
                    all_pieces := deep_copy_all_pieces !two_rounds_ago_setup;
                    two_rounds_ago_setup := deep_copy_all_pieces !all_pieces;
                    one_rounds_ago_setup := deep_copy_all_pieces !all_pieces)
                  else if i = 3 && !can_report_wu then (
                    report := "report_wu";
                    can_report_wu := false;
                    can_report_shu := false;
                    can_report := false;
                    decr turn_counter;
                    all_pieces := deep_copy_all_pieces !one_rounds_ago_setup;
                    one_rounds_ago_setup :=
                      deep_copy_all_pieces !two_rounds_ago_setup;
                    two_rounds_ago_setup := deep_copy_all_pieces !all_pieces;
                    existing_kings := !existing_kings_one_round_ago;
                    existing_kings_one_round_ago :=
                      !existing_kings_two_rounds_ago;
                    existing_kings_two_rounds_ago := !existing_kings)
                  else report := "You cannot report yourself."
                else if current_turn () = Orange && !can_report then
                  if i = 1 && !can_report_wei then (
                    report := "report_wei";
                    can_report_wei := false;
                    can_report_wu := false;
                    can_report := false;
                    decr turn_counter;
                    all_pieces := deep_copy_all_pieces !one_rounds_ago_setup;
                    one_rounds_ago_setup :=
                      deep_copy_all_pieces !two_rounds_ago_setup;
                    two_rounds_ago_setup := deep_copy_all_pieces !all_pieces)
                  else if i = 3 && !can_report_wu && !turn_counter > 1 then (
                    report := "report_wu";
                    can_report_wu := false;
                    can_report_wei := false;
                    can_report := false;
                    decr turn_counter;
                    decr turn_counter;
                    all_pieces := deep_copy_all_pieces !two_rounds_ago_setup;
                    two_rounds_ago_setup := deep_copy_all_pieces !all_pieces;
                    one_rounds_ago_setup := deep_copy_all_pieces !all_pieces;
                    existing_kings := !existing_kings_two_rounds_ago;
                    existing_kings_two_rounds_ago := !existing_kings;
                    existing_kings_one_round_ago := !existing_kings)
                  else report := "You cannot report yourself."
                else if current_turn () = Yellow && !can_report then
                  if i = 1 && !can_report_wei && !turn_counter > 1 then (
                    report := "report_wei";
                    can_report_wei := false;
                    can_report_shu := false;
                    can_report := false;
                    decr turn_counter;
                    decr turn_counter;
                    all_pieces := deep_copy_all_pieces !two_rounds_ago_setup;
                    two_rounds_ago_setup := deep_copy_all_pieces !all_pieces;
                    one_rounds_ago_setup := deep_copy_all_pieces !all_pieces;
                    existing_kings := !existing_kings_two_rounds_ago;
                    existing_kings_two_rounds_ago := !existing_kings;
                    existing_kings_one_round_ago := !existing_kings)
                  else if i = 2 && !can_report_shu then (
                    report := "report_shu";
                    can_report_shu := false;
                    can_report_wei := false;
                    can_report := false;
                    decr turn_counter;
                    all_pieces := deep_copy_all_pieces !one_rounds_ago_setup;
                    one_rounds_ago_setup :=
                      deep_copy_all_pieces !two_rounds_ago_setup;
                    two_rounds_ago_setup := deep_copy_all_pieces !all_pieces;
                    existing_kings := !existing_kings_one_round_ago;
                    existing_kings_one_round_ago :=
                      !existing_kings_two_rounds_ago;
                    existing_kings_two_rounds_ago := !existing_kings)
                  else report := "You cannot report yourself."
                else report := "Consecutive report is not allowed"
              else
                let center_x, center_y =
                  BoardFunctions.board_center_points c i
                in
                Array.iter
                  (fun pieces ->
                    Array.iter
                      (fun piece ->
                        if piece.picked then begin
                          if
                            not
                              (square_contains_same_color_piece !all_pieces
                                 piece.color (c, i))
                            (* the piece can move *)
                          then begin
                            two_rounds_ago_setup :=
                              deep_copy_all_pieces !one_rounds_ago_setup;
                            one_rounds_ago_setup :=
                              deep_copy_all_pieces !all_pieces;
                            existing_kings_two_rounds_ago :=
                              !existing_kings_one_round_ago;
                            existing_kings_one_round_ago := !existing_kings;

                            report := "";
                            if
                              (* the piece move to capture other's piece*)
                              square_contains_opponent_piece !all_pieces
                                piece.color (c, i)
                            then (
                              (* the piece move to empty grid*)
                              let captured_piece =
                                find_captured_piece !all_pieces
                                  (center_x, center_y)
                              in
                              all_pieces :=
                                remove_piece_at_position !all_pieces
                                  (center_x, center_y);
                              piece.position <- (center_x, center_y);
                              piece.picked <- false;
                              is_piece_picked := true;

                              incr turn_counter;
                              can_report := true;
                              if piece.color = Red then can_report_wei := true
                              else if piece.color = Orange then
                                can_report_shu := true
                              else can_report_wu := true;

                              (* Check if the captured piece is a king *)
                              match captured_piece with
                              | Some p when p.kind = King ->
                                  eliminate_king_and_take_over piece.color
                                    p.color !all_pieces;
                                  (* Adjust turn_counter based on the game's new
                                     state *)
                                  begin
                                    let adjust_turn_counter () =
                                      match (piece.color, p.color) with
                                      | Red, Orange | Red, Yellow ->
                                          turn_counter := 1
                                      | Orange, Red | Orange, Yellow ->
                                          turn_counter := 1
                                      | Yellow, Red | Yellow, Orange ->
                                          turn_counter := 0
                                      | _ -> ()
                                    in

                                    adjust_turn_counter ()
                                  end;
                                  begin
                                    match check_king_winner existing_kings with
                                    | Some color ->
                                        game_over := true;
                                        game_started := false;
                                        winning_side := Some color
                                    (* The side of the capturing piece *)
                                    | None -> ()
                                  end
                              | _ -> ())
                            else begin
                              if piece.kind = King && not piece.has_moved then begin
                                let find_rook_for_castling king_pos =
                                  Array.find_opt
                                    (fun p ->
                                      p.kind = Rook && p.color = piece.color
                                      && p.position
                                         =
                                         match king_pos with
                                         | 'G', 1 ->
                                             BoardFunctions.board_center_points
                                               'H' 1
                                         | 'C', 1 ->
                                             BoardFunctions.board_center_points
                                               'A' 1
                                         | 'K', 12 ->
                                             BoardFunctions.board_center_points
                                               'L' 12
                                         | 'F', 12 ->
                                             BoardFunctions.board_center_points
                                               'H' 12
                                         | 'B', 8 ->
                                             BoardFunctions.board_center_points
                                               'A' 8
                                         | 'J', 8 ->
                                             BoardFunctions.board_center_points
                                               'L' 8
                                         | _ -> piece.position)
                                    (Array.concat (Array.to_list !all_pieces))
                                in
                                match find_rook_for_castling (c, i) with
                                | Some rook_piece
                                  when can_castle piece rook_piece !all_pieces
                                  ->
                                    (*Printf.printf "Castling with %s rook\n"
                                      (color_to_string rook_piece.color);*)
                                    let king_new_pos, rook_new_pos =
                                      get_castling_new_positions piece.color
                                        (c, i)
                                    in
                                    let x_king, y_king =
                                      BoardFunctions.board_center_points
                                        (fst king_new_pos) (snd king_new_pos)
                                    in
                                    let x_rook, y_rook =
                                      BoardFunctions.board_center_points
                                        (fst rook_new_pos) (snd rook_new_pos)
                                    in
                                    piece.position <- (x_king, y_king);
                                    (* Move king to its new position *)
                                    rook_piece.position <- (x_rook, y_rook);
                                    (* Move rook to its new position *)
                                    piece.has_moved <- true;
                                    rook_piece.has_moved <- true;
                                    piece.picked <- false;
                                    is_piece_picked := false;
                                    can_report := true;
                                    if piece.color = Red then
                                      can_report_wei := true
                                    else if piece.color = Orange then
                                      can_report_shu := true
                                    else can_report_wu := true;
                                    incr turn_counter
                                | Some _ | None ->
                                    (*Printf.printf "Castling not possible or
                                      rook not found\n";*)
                                    piece.position <- (center_x, center_y);
                                    piece.picked <- false;
                                    is_piece_picked := false;
                                    incr turn_counter;
                                    piece.has_moved <- true;
                                    can_report := true;
                                    if piece.color = Red then
                                      can_report_wei := true
                                    else if piece.color = Orange then
                                      can_report_shu := true
                                    else can_report_wu := true
                              end
                              else begin
                                piece.position <- (center_x, center_y);
                                piece.picked <- false;
                                is_piece_picked := false;
                                incr turn_counter;
                                piece.has_moved <- true;
                                can_report := true;
                                if piece.color = Red then can_report_wei := true
                                else if piece.color = Orange then
                                  can_report_shu := true
                                else can_report_wu := true
                              end
                            end;

                            (* check if certain player has occupied all grids
                               required to win*)
                            let red_win_required_squares =
                              [| ('D', 5); ('I', 5); ('I', 9); ('E', 9) |]
                            in
                            let orange_win_required_squares =
                              [| ('D', 5); ('I', 5); ('D', 4); ('E', 4) |]
                            in
                            let yellow_win_required_squares =
                              [| ('D', 4); ('E', 4); ('I', 9); ('E', 9) |]
                            in
                            match piece.color with
                            | Red ->
                                if
                                  occupied_1_of_win_squares !all_pieces
                                    red_win_required_squares Red
                                then (
                                  game_over := true;
                                  game_started := false;
                                  winning_side := Some piece.color)
                            | Orange ->
                                if
                                  occupied_1_of_win_squares !all_pieces
                                    orange_win_required_squares Orange
                                then (
                                  game_over := true;
                                  game_started := false;
                                  winning_side := Some piece.color)
                            | Yellow ->
                                if
                                  occupied_1_of_win_squares !all_pieces
                                    yellow_win_required_squares Yellow
                                then (
                                  game_over := true;
                                  game_started := false;
                                  winning_side := Some piece.color)
                          end
                          else begin
                            piece.picked <- false
                          end
                        end
                        else if not !is_piece_picked then
                          (* Only pick a pawn if none is currently picked *)
                          let dist =
                            Vector2.distance mouse_position
                              (Vector2.create (fst piece.position)
                                 (snd piece.position))
                          in
                          if dist < 20.0 && piece.color = current_turn () then begin
                            piece.picked <- true;
                            is_piece_picked :=
                              true (* Indicate a pawn is now picked *)
                          end)
                      pieces)
                  !all_pieces
          | None ->
              is_piece_picked := false;
              Array.iter
                (fun pieces ->
                  Array.iter (fun pawn -> pawn.picked <- false) pieces)
                !all_pieces
        end
      end;
      Array.iter
        (fun pieces -> Array.iter (fun piece -> Pieces.render piece) pieces)
        !all_pieces;

      if not !game_started then begin
        List.iteri
          (fun i line ->
            let y_position = padding + (i * line_height) in
            draw_text line x_position y_position line_height
              (Color.create 0 0 0 255))
          (if (not !game_started) && not !game_over then messages1
           else messages2)
      end;

      if !game_started then
        draw_text !display_string 10 10 20 (Color.create 0 0 255 255);

      (* for debugging: *)
      (* draw_text !position_str 100 10 20 (Color.create 0 0 255 50); *)
      draw_text !report 950 40 20 (Color.create 0 0 255 255);
      (* draw_text (Printf.sprintf "[%s, %s]" (string_of_float (Vector2.x
         mouse_position)) (string_of_float (Vector2.y mouse_position))) 1160 700
         20 (Color.create 0 0 0 30); *)
      draw_text
        (if !game_over then "game over!!!" else "game ongoing...")
        10 160 20 (Color.create 0 0 255 255);
      draw_text
        ("#turn: " ^ string_of_int !turn_counter)
        10 200 20 (Color.create 0 0 255 255);
      (* (let red_win_required_squares = [| ('D', 5); ('I', 5); ('I', 9); ('E',
         9) |] in draw_text (if occupied_1_of_win_squares !all_pieces
         red_win_required_squares Red then "should over" else "should not") 10
         200 20 (Color.create 0 0 255 255)); *)
      (let printreporttext current_turn_color =
         if current_turn_color = Red then (
           draw_text "Report foul" 1030 140 20 shu_color;
           draw_text "Report foul" 150 140 20 wu_color)
         else if current_turn_color = Orange then (
           draw_text "Report foul" 150 580 20 wei_color;
           draw_text "Report foul" 150 140 20 wu_color)
         else if current_turn_color = Yellow then (
           draw_text "Report foul" 1030 140 20 shu_color;
           draw_text "Report foul" 150 580 20 wei_color)
         else
           draw_text "printreporttext: unexpected color received" 1030 600 20
             wei_color
       in
       if !game_started then
         match current_turn () with
         | Red -> printreporttext Red
         | Orange -> printreporttext Orange
         | Yellow -> printreporttext Yellow);

      let turn_string, turn_color =
        match current_turn () with
        | Red -> ("Wei's turn", wei_color)
        | Orange -> ("Shu's turn", shu_color)
        | Yellow -> ("Wu's turn", wu_color)
      in
      if !game_started then draw_text turn_string 10 60 20 turn_color;

      end_drawing ()
    end
  done;

  unload_textures ();
  unload_texture board_texture;

  close_window ()
