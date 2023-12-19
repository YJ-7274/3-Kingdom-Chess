open Raylib

type color =
  | Red
  | Yellow
  | Orange

type piece_kind =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type piece = {
  id : int;
  kind : piece_kind;
  mutable color : color;
  mutable position : float * float;
  mutable picked : bool;
  mutable has_moved : bool;
}

let piece_id_counter = ref 0

let generate_unique_id () =
  let id = !piece_id_counter in
  incr piece_id_counter;
  id

(* Global reference for the red pawn texture *)
let red_pawn_texture_ref = ref None
let red_king_texture_ref = ref None
let red_queen_texture_ref = ref None
let red_bishop_texture_ref = ref None
let red_knight_texture_ref = ref None
let red_rook_texture_ref = ref None
let yellow_pawn_texture_ref = ref None
let yellow_king_texture_ref = ref None
let yellow_queen_texture_ref = ref None
let yellow_bishop_texture_ref = ref None
let yellow_knight_texture_ref = ref None
let yellow_rook_texture_ref = ref None
let orange_pawn_texture_ref = ref None
let orange_king_texture_ref = ref None
let orange_queen_texture_ref = ref None
let orange_bishop_texture_ref = ref None
let orange_knight_texture_ref = ref None
let orange_rook_texture_ref = ref None

let texture_of_piece piece =
  let prefix =
    match piece.color with
    | Red -> "r"
    | Yellow -> "y"
    | Orange -> "o"
  in
  let piece_name =
    match piece.kind with
    | Pawn -> "pawn"
    | Knight -> "knight"
    | Bishop -> "bishop"
    | Rook -> "rook"
    | Queen -> "queen"
    | King -> "king"
  in
  let texture_path = Printf.sprintf "pieces/%s_%s.png" prefix piece_name in

  match piece.kind with
  | Pawn -> begin
      match piece.color with
      | Red -> begin
          match !red_pawn_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              red_pawn_texture_ref := Some tex;
              tex
        end
      | Yellow -> begin
          match !yellow_pawn_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              yellow_pawn_texture_ref := Some tex;
              tex
        end
      | Orange -> begin
          match !orange_pawn_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              orange_pawn_texture_ref := Some tex;
              tex
        end
    end
  | King -> begin
      match piece.color with
      | Red -> begin
          match !red_king_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              red_king_texture_ref := Some tex;
              tex
        end
      | Yellow -> begin
          match !yellow_king_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              yellow_king_texture_ref := Some tex;
              tex
        end
      | Orange -> begin
          match !orange_king_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              orange_king_texture_ref := Some tex;
              tex
        end
    end
  | Queen -> begin
      match piece.color with
      | Red -> begin
          match !red_queen_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              red_queen_texture_ref := Some tex;
              tex
        end
      | Yellow -> begin
          match !yellow_queen_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              yellow_queen_texture_ref := Some tex;
              tex
        end
      | Orange -> begin
          match !orange_queen_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              orange_queen_texture_ref := Some tex;
              tex
        end
    end
  | Bishop -> begin
      match piece.color with
      | Red -> begin
          match !red_bishop_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              red_bishop_texture_ref := Some tex;
              tex
        end
      | Yellow -> begin
          match !yellow_bishop_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              yellow_bishop_texture_ref := Some tex;
              tex
        end
      | Orange -> begin
          match !orange_bishop_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              orange_bishop_texture_ref := Some tex;
              tex
        end
    end
  | Knight -> begin
      match piece.color with
      | Red -> begin
          match !red_knight_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              red_knight_texture_ref := Some tex;
              tex
        end
      | Yellow -> begin
          match !yellow_knight_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              yellow_knight_texture_ref := Some tex;
              tex
        end
      | Orange -> begin
          match !orange_knight_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              orange_knight_texture_ref := Some tex;
              tex
        end
    end
  | Rook -> begin
      match piece.color with
      | Red -> begin
          match !red_rook_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              red_rook_texture_ref := Some tex;
              tex
        end
      | Yellow -> begin
          match !yellow_rook_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              yellow_rook_texture_ref := Some tex;
              tex
        end
      | Orange -> begin
          match !orange_rook_texture_ref with
          | Some tex -> tex
          | None ->
              let tex = load_texture texture_path in
              orange_rook_texture_ref := Some tex;
              tex
        end
    end

let unload_textures () =
  (* Unload the red pawn texture if it was loaded *)
  match !red_pawn_texture_ref with
  | Some tex -> unload_texture tex
  | None -> ()

let render piece =
  let texture = texture_of_piece piece in
  (* Corrected this line *)
  let width = 40 in
  let height = 40 in
  let draw_pos_x = fst piece.position -. (float_of_int width /. 2.0) in
  let draw_pos_y = snd piece.position -. (float_of_int height /. 2.0) in
  let dest_rect =
    Rectangle.create draw_pos_x draw_pos_y (float_of_int width)
      (float_of_int height)
  in
  let origin = Vector2.create 0.0 0.0 in
  draw_texture_pro texture
    (Rectangle.create 0.0 0.0 (float_of_int width) (float_of_int height))
    dest_rect origin 0.0
    (Color.create 255 255 255 255)

module Pawn = struct
  let create color =
    {
      id = generate_unique_id ();
      kind = Pawn;
      color;
      position = (0.0, 0.0);
      picked = false;
      has_moved = false;
    }

  let render pawn = render pawn
end

module Knight = struct
  let create color =
    {
      id = generate_unique_id ();
      kind = Knight;
      color;
      position = (0.0, 0.0);
      picked = false;
      has_moved = false;
    }

  let render knight = render knight
end

module Rook = struct
  let create color =
    {
      id = generate_unique_id ();
      kind = Rook;
      color;
      position = (0.0, 0.0);
      picked = false;
      has_moved = false;
    }

  let render rook = render rook
end

module Bishop = struct
  let create color =
    {
      id = generate_unique_id ();
      kind = Bishop;
      color;
      position = (0.0, 0.0);
      picked = false;
      has_moved = false;
    }

  let render bishop = render bishop
end

module Queen = struct
  let create color =
    {
      id = generate_unique_id ();
      kind = Queen;
      color;
      position = (0.0, 0.0);
      picked = false;
      has_moved = false;
    }

  let render queen = render queen
end

module King = struct
  let create color =
    {
      id = generate_unique_id ();
      kind = King;
      color;
      position = (0.0, 0.0);
      picked = false;
      has_moved = false;
    }

  let render king = render king
end
