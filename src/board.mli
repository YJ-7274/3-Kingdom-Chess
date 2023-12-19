module type BoardFunctionsType = sig
  val mouse_position_to_square : float -> float -> (char * int) option
  val board_center_points : char -> int -> float * float
  val split_first_character : string -> string * string

  val square_to_mouse_position :
    string option ->
    ((float * float) * (float * float) * (float * float) * (float * float))
    option

  val square_to_string : (char * int) option -> string
end

module BoardFunctions : BoardFunctionsType
