open OUnit2

(* open Chess *)
(* open Board *)

(***************************Board tests**************************************)
(* let board_center_points_test = Board.board_center_points_tests 'I' 5 *)
let board_center_points_tests = []
let split_first_character_tests = []
let board_tests = board_center_points_tests @ split_first_character_tests

(***************************All tests**************************************)

let suite = "test suite for 3KChess" >::: List.flatten [ board_tests ]
let () = run_test_tt_main suite
