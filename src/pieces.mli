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

val texture_of_piece : piece -> Texture2D.t
val unload_textures : unit -> unit
val render : piece -> unit

module Pawn : sig
  val create : color -> piece
  val render : piece -> unit
end

module Knight : sig
  val create : color -> piece
  val render : piece -> unit
end

module Rook : sig
  val create : color -> piece
  val render : piece -> unit
end

module Bishop : sig
  val create : color -> piece
  val render : piece -> unit
end

module Queen : sig
  val create : color -> piece
  val render : piece -> unit
end

module King : sig
  val create : color -> piece
  val render : piece -> unit
end
