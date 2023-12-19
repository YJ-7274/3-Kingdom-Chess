let screen_width = 1280
let screen_height = 720

let is_point_inside_triangle (p, q, r) (x, y) =
  let area =
    0.5
    *. abs_float
         (((p |> fst) *. ((q |> snd) -. (r |> snd)))
         +. ((q |> fst) *. ((r |> snd) -. (p |> snd)))
         +. ((r |> fst) *. ((p |> snd) -. (q |> snd))))
  in
  let area1 =
    0.5
    *. abs_float
         ((x *. ((q |> snd) -. (r |> snd)))
         +. ((q |> fst) *. ((r |> snd) -. y))
         +. ((r |> fst) *. (y -. (q |> snd))))
  in
  let area2 =
    0.5
    *. abs_float
         (((p |> fst) *. (y -. (r |> snd)))
         +. (x *. ((r |> snd) -. (p |> snd)))
         +. ((r |> fst) *. ((p |> snd) -. y)))
  in
  let area3 =
    0.5
    *. abs_float
         (((p |> fst) *. ((q |> snd) -. y))
         +. ((q |> fst) *. (y -. (p |> snd)))
         +. (x *. ((p |> snd) -. (q |> snd))))
  in
  abs_float (area1 +. area2 +. area3 -. area)
  = 0. (* Tolerance for floating-point comparisons *)

(**inputs: top left, top right, bottom right, bottom left*)
let coord_in_quadrilateral (x, y) (p1, p2, p3, p4) =
  is_point_inside_triangle (p1, p2, p3) (x, y)
  || is_point_inside_triangle (p1, p3, p4) (x, y)

(**inputs: top left, top right, bottom left, bottom right. this fcn simply
   changes the inputs' sequence*)
let ciq_tl_tr_bl_br (x, y) (p1, p2, p3, p4) =
  coord_in_quadrilateral (x, y) (p1, p2, p4, p3)

(*inputs: top left, top right, bottom right, bottom left*)
(*returns the center point of the four corners*)
let centerpoint ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) : float * float =
  let x_sum = x1 +. x2 +. x3 +. x4 in
  let y_sum = y1 +. y2 +. y3 +. y4 in
  ( (x_sum /. 4.0) +. float_of_int (screen_width / 2),
    (y_sum /. 4.0) +. float_of_int (screen_height / 2) )

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

module BoardFunctions : BoardFunctionsType = struct
  (** mouse_position_to_square x y return the square in which the point (x,y) is
      in *)
  let mouse_position_to_square (x : float) (y : float) : (char * int) option =
    if
      coord_in_quadrilateral (x, y)
        ((-214., 294.), (-161., 283.), (-144., 334.), (-191., 334.))
    then Some ('A', 1)
    else if
      coord_in_quadrilateral (x, y)
        ((-238., 251.), (-179., 232.), (-161., 283.), (-214., 294.))
    then Some ('A', 2)
    else if
      coord_in_quadrilateral (x, y)
        ((-262., 210.), (-197., 179.), (-179., 232.), (-238., 251.))
    then Some ('A', 3)
    else if
      coord_in_quadrilateral (x, y)
        ((-286., 168.), (-215., 127.), (-197., 179.), (-262., 210.))
    then Some ('A', 4)
    else if
      coord_in_quadrilateral (x, y)
        ((-311., 127.), (-251., 85.), (-215., 127.), (-286., 168.))
    then Some ('A', 5)
    else if
      coord_in_quadrilateral (x, y)
        ((-334., 85.), (-287., 44.), (-251., 85.), (-311., 127.))
    then Some ('A', 6)
    else if
      coord_in_quadrilateral (x, y)
        ((-359., 43.), (-323., 2.), (-287., 44.), (-334., 85.))
    then Some ('A', 7)
    else if
      coord_in_quadrilateral (x, y)
        ((-383., 2.), (-359., -39.), (-323., 2.), (-359., 43.))
    then Some ('A', 8)
    else if
      coord_in_quadrilateral (x, y)
        ((-161., 283.), (-107., 272.), (-95., 334.), (-143., 334.))
    then Some ('B', 1)
    else if
      coord_in_quadrilateral (x, y)
        ((-178., 231.), (-119., 211.), (-107., 272.), (-161., 283.))
    then Some ('B', 2)
    else if
      coord_in_quadrilateral (x, y)
        ((-197., 179.), (-131., 148.), (-119., 211.), (-178., 231.))
    then Some ('B', 3)
    else if
      coord_in_quadrilateral (x, y)
        ((-214., 127.), (-144., 86.), (-131., 148.), (-197., 179.))
    then Some ('B', 4)
    else if
      coord_in_quadrilateral (x, y)
        ((-250., 85.), (-191., 44.), (-144., 86.), (-214., 127.))
    then Some ('B', 5)
    else if
      coord_in_quadrilateral (x, y)
        ((-285., 44.), (-239., 4.), (-191., 44.), (-250., 85.))
    then Some ('B', 6)
    else if
      coord_in_quadrilateral (x, y)
        ((-322., 3.), (-288., -37.), (-239., 4.), (-285., 44.))
    then Some ('B', 7)
    else if
      coord_in_quadrilateral (x, y)
        ((-358., -38.), (-334., -79.), (-288., -37.), (-322., 3.))
    then Some ('B', 8)
    else if
      coord_in_quadrilateral (x, y)
        ((-106., 274.), (-54., 264.), (-49., 332.), (-94., 332.))
    then Some ('C', 1)
    else if
      coord_in_quadrilateral (x, y)
        ((-118., 210.), (-60., 190.), (-54., 264.), (-106., 274.))
    then Some ('C', 2)
    else if
      coord_in_quadrilateral (x, y)
        ((-130., 149.), (-66., 118.), (-60., 190.), (-118., 210.))
    then Some ('C', 3)
    else if
      coord_in_quadrilateral (x, y)
        ((-142., 86.), (-71., 45.), (-66., 118.), (-130., 149.))
    then Some ('C', 4)
    else if
      coord_in_quadrilateral (x, y)
        ((-189., 44.), (-131., 3.), (-71., 45.), (-142., 86.))
    then Some ('C', 5)
    else if
      coord_in_quadrilateral (x, y)
        ((-237., 2.), (-191., -38.), (-131., 3.), (-189., 44.))
    then Some ('C', 6)
    else if
      coord_in_quadrilateral (x, y)
        ((-285., -38.), (-251., -79.), (-191., -38.), (-237., 2.))
    then Some ('C', 7)
    else if
      coord_in_quadrilateral (x, y)
        ((-333., -81.), (-310., -121.), (-252., -80.), (-287., -41.))
    then Some ('C', 8)
    else if
      coord_in_quadrilateral (x, y)
        ((-52., 262.), (0., 252.), (0., 333.), (-47., 332.))
    then Some ('D', 1)
    else if
      coord_in_quadrilateral (x, y)
        ((-58., 190.), (0., 170.), (0., 252.), (-52., 262.))
    then Some ('D', 2)
    else if
      coord_in_quadrilateral (x, y)
        ((-64., 117.), (0., 87.), (0., 170.), (-58., 190.))
    then Some ('D', 3)
    else if
      coord_in_quadrilateral (x, y)
        ((-70., 45.), (0., 3.), (0., 87.), (-64., 117.))
    then Some ('D', 4)
    else if
      coord_in_quadrilateral (x, y)
        ((-128., 2.), (-71., -36.), (0., 3.), (-70., 45.))
    then Some ('D', 5)
    else if
      coord_in_quadrilateral (x, y)
        ((-190., -39.), (-143., -79.), (-71., -36.), (-128., 2.))
    then Some ('D', 6)
    else if
      coord_in_quadrilateral (x, y)
        ((-250., -81.), (-215., -121.), (-143., -79.), (-190., -39.))
    then Some ('D', 7)
    else if
      coord_in_quadrilateral (x, y)
        ((-310., -122.), (-286., -162.), (-215., -121.), (-250., -81.))
    then Some ('D', 8) (*Yanjun Section*)
    else if
      coord_in_quadrilateral (x, y)
        ((0., 248.), (49., 261.), (43., 326.), (1., 327.))
    then Some ('E', 1)
    else if
      coord_in_quadrilateral (x, y)
        ((1., 169.), (55., 187.), (50., 255.), (1., 245.))
    then Some ('E', 2)
    else if
      coord_in_quadrilateral (x, y)
        ((0., 86.), (60., 115.), (52., 178.), (1., 161.))
    then Some ('E', 3)
    else if
      coord_in_quadrilateral (x, y)
        ((1., 1.), (67., 41.), (62., 109.), (0., 79.))
    then Some ('E', 4)
    else if
      coord_in_quadrilateral (x, y)
        ((3., -2.), (69., -40.), (121., -3.), (70., 33.))
    then Some ('E', 9)
    else if
      coord_in_quadrilateral (x, y)
        ((77., -42.), (141., -81.), (183., -45.), (131., -8.))
    then Some ('E', 10)
    else if
      coord_in_quadrilateral (x, y)
        ((149., -83.), (214., -121.), (244., -85.), (192., -50.))
    then Some ('E', 11)
    else if
      coord_in_quadrilateral (x, y)
        ((218., -127.), (286., -164.), (303., -128.), (251., -90.))
    then Some ('E', 12)
    else if
      coord_in_quadrilateral (x, y)
        ((54., 261.), (102., 273.), (88., 327.), (48., 327.))
    then Some ('F', 1)
    else if
      coord_in_quadrilateral (x, y)
        ((60., 190.), (115., 210.), (103., 265.), (55., 255.))
    then Some ('F', 2)
    else if
      coord_in_quadrilateral (x, y)
        ((66., 117.), (125., 146.), (115., 200.), (61., 183.))
    then Some ('F', 3)
    else if
      coord_in_quadrilateral (x, y)
        ((71., 43.), (138., 83.), (128., 141.), (67., 111.))
    then Some ('F', 4)
    else if
      coord_in_quadrilateral (x, y)
        ((76., 40.), (128., 1.), (183., 40.), (140., 77.))
    then Some ('F', 9)
    else if
      coord_in_quadrilateral (x, y)
        ((135., -2.), (190., -40.), (230., -4.), (185., 34.))
    then Some ('F', 10)
    else if
      coord_in_quadrilateral (x, y)
        ((196., -43.), (250., -80.), (280., -44.), (237., -8.))
    then Some ('F', 11)
    else if
      coord_in_quadrilateral (x, y)
        ((256., -86.), (308., -121.), (327., -86.), (286., -49.))
    then Some ('F', 12)
    else if
      coord_in_quadrilateral (x, y)
        ((107., 272.), (156., 279.), (138., 326.), (98., 328.))
    then Some ('G', 1)
    else if
      coord_in_quadrilateral (x, y)
        ((120., 210.), (172., 228.), (157., 274.), (109., 265.))
    then Some ('G', 2)
    else if
      coord_in_quadrilateral (x, y)
        ((132., 150.), (189., 177.), (175., 221.), (121., 204.))
    then Some ('G', 3)
    else if
      coord_in_quadrilateral (x, y)
        ((144., 86.), (212., 124.), (193., 169.), (135., 140.))
    then Some ('G', 4)
    else if
      coord_in_quadrilateral (x, y)
        ((149., 81.), (188., 43.), (240., 83.), (212., 116.))
    then Some ('G', 9)
    else if
      coord_in_quadrilateral (x, y)
        ((195., 40.), (236., 3.), (281., 40.), (250., 76.))
    then Some ('G', 10)
    else if
      coord_in_quadrilateral (x, y)
        ((243., -2.), (285., -36.), (314., -4.), (285., 33.))
    then Some ('G', 11)
    else if
      coord_in_quadrilateral (x, y)
        ((290., -43.), (332., -80.), (352., -43.), (323., -6.))
    then Some ('G', 12)
    else if
      coord_in_quadrilateral (x, y)
        ((162., 281.), (207., 292.), (188., 325.), (148., 325.))
    then Some ('H', 1)
    else if
      coord_in_quadrilateral (x, y)
        ((179., 231.), (231., 247.), (212., 284.), (164., 274.))
    then Some ('H', 2)
    else if
      coord_in_quadrilateral (x, y)
        ((198., 182.), (256., 207.), (235., 238.), (182., 223.))
    then Some ('H', 3)
    else if
      coord_in_quadrilateral (x, y)
        ((215., 127.), (280., 165.), (258., 200.), (200., 173.))
    then Some ('H', 4)
    else if
      coord_in_quadrilateral (x, y)
        ((220., 123.), (251., 87.), (303., 123.), (283., 159.))
    then Some ('H', 9)
    else if
      coord_in_quadrilateral (x, y)
        ((258., 80.), (287., 44.), (329., 79.), (307., 117.))
    then Some ('H', 10)
    else if
      coord_in_quadrilateral (x, y)
        ((293., 41.), (322., 7.), (354., 39.), (332., 75.))
    then Some ('H', 11)
    else if
      coord_in_quadrilateral (x, y)
        ((325., -2.), (356., -39.), (376., -3.), (355., 31.))
    then Some ('H', 12)
      (***** Mark's part below: the top 1/3 of the chess board*****)
      (*** L's row. ***)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-192., -331.), (-144., -331.), (-217., -290.), (-162., -280.))
    then Some ('L', 8)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-144., -331.), (-97., -331.), (-162., -280.), (-109., -270.))
    then Some ('L', 7)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-97., -331.), (-49., -331.), (-109., -270.), (-54., -260.))
    then Some ('L', 6)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-49., -331.), (0., -331.), (-54., -260.), (0., -250.))
    then Some ('L', 5)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((0., -331.), (49., -331.), (0., -250.), (54., -260.))
    then Some ('L', 9)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((49., -331.), (97., -331.), (54., -260.), (109., -270.))
    then Some ('L', 10)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((97., -331.), (144., -331.), (109., -270.), (162., -280.))
    then Some ('L', 11)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((144., -331.), (192., -331.), (162., -280.), (217., -290.))
    then Some ('L', 12) (**** K's row. ****)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-217., -290.), (-162., -280.), (-240., -250.), (-180., -228.))
    then Some ('K', 8)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-162., -280.), (-109., -270.), (-180., -228.), (-120., -207.))
    then Some ('K', 7)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-109., -270.), (-54., -260.), (-120., -207.), (-60., -187.))
    then Some ('K', 6)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-54., -260.), (0., -250.), (-60., -187.), (0., -165.))
    then Some ('K', 5)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((0., -250.), (54., -260.), (0., -165.), (60., -187.))
    then Some ('K', 9)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((54., -260.), (109., -270.), (60., -187.), (120., -207.))
    then Some ('K', 10)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((109., -270.), (162., -280.), (120., -207.), (180., -228.))
    then Some ('K', 11)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((162., -280.), (217., -290.), (180., -228.), (240., -250.))
    then Some ('K', 12) (*** J's row. ***)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-240., -250.), (-180., -228.), (-246., -206.), (-200., -177.))
    then Some ('J', 8)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-180., -228.), (-120., -207.), (-200., -177.), (-132., -145.))
    then Some ('J', 7)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-120., -207.), (-60., -187.), (-132., -145.), (-66., -113.))
    then Some ('J', 6)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-60., -187.), (0., -165.), (-66., -113.), (0., -83.))
    then Some ('J', 5)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((0., -165.), (60., -187.), (0., -83.), (66., -113.))
    then Some ('J', 9)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((60., -187.), (120., -207.), (66., -113.), (132., -145.))
    then Some ('J', 10)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((120., -207.), (180., -228.), (132., -145.), (200., -177.))
    then Some ('J', 11)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((180., -228.), (240., -250.), (200., -177.), (246., -206.))
    then Some ('J', 12) (*** I's row. ***)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-246., -206.), (-200., -177.), (-288., -165.), (-217., -124.))
    then Some ('I', 8)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-200., -177.), (-132., -145.), (-217., -124.), (-144., -82.))
    then Some ('I', 7)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-132., -145.), (-66., -113.), (-144., -82.), (-72., -41.))
    then Some ('I', 6)
    else if
      ciq_tl_tr_bl_br (x, y) ((-66., -113.), (0., -83.), (-72., -41.), (0., 0.))
    then Some ('I', 5)
    else if
      ciq_tl_tr_bl_br (x, y) ((0., -83.), (66., -113.), (0., 0.), (72., -41.))
    then Some ('I', 9)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((66., -113.), (132., -145.), (72., -41.), (144., -82.))
    then Some ('I', 10)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((132., -145.), (200., -177.), (144., -82.), (217., -124.))
    then Some ('I', 11)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((200., -177.), (246., -206.), (217., -124.), (288., -165.))
    then Some ('I', 12)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-490., 220.), (-370., 220.), (-490., 240.), (-370., 240.))
    then Some ('R', 1)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((390., -220.), (510., -220.), (390., -200.), (510., -200.))
    then Some ('R', 2)
    else if
      ciq_tl_tr_bl_br (x, y)
        ((-490., -220.), (-370., -220.), (-490., -200.), (-370., -200.))
    then Some ('R', 3)
    else None

  let board_center_points (letter : char) (number : int) : float * float =
    match (letter, number) with
    | 'A', 1 ->
        centerpoint ((-214., 294.), (-161., 283.), (-144., 334.), (-191., 334.))
    | 'A', 2 ->
        centerpoint ((-238., 251.), (-179., 232.), (-161., 283.), (-214., 294.))
    | 'A', 3 ->
        centerpoint ((-262., 210.), (-197., 179.), (-179., 232.), (-238., 251.))
    | 'A', 4 ->
        centerpoint ((-286., 168.), (-215., 127.), (-197., 179.), (-262., 210.))
    | 'A', 5 ->
        centerpoint ((-311., 127.), (-251., 85.), (-215., 127.), (-286., 168.))
    | 'A', 6 ->
        centerpoint ((-334., 85.), (-287., 44.), (-251., 85.), (-311., 127.))
    | 'A', 7 ->
        centerpoint ((-359., 43.), (-323., 2.), (-287., 44.), (-334., 85.))
    | 'A', 8 ->
        centerpoint ((-383., 2.), (-359., -39.), (-323., 2.), (-359., 43.))
    | 'B', 1 ->
        centerpoint ((-161., 283.), (-107., 272.), (-95., 334.), (-143., 334.))
    | 'B', 2 ->
        centerpoint ((-178., 231.), (-119., 211.), (-107., 272.), (-161., 283.))
    | 'B', 3 ->
        centerpoint ((-197., 179.), (-131., 148.), (-119., 211.), (-178., 231.))
    | 'B', 4 ->
        centerpoint ((-214., 127.), (-144., 86.), (-131., 148.), (-197., 179.))
    | 'B', 5 ->
        centerpoint ((-250., 85.), (-191., 44.), (-144., 86.), (-214., 127.))
    | 'B', 6 ->
        centerpoint ((-285., 44.), (-239., 4.), (-191., 44.), (-250., 85.))
    | 'B', 7 ->
        centerpoint ((-322., 3.), (-288., -37.), (-239., 4.), (-285., 44.))
    | 'B', 8 ->
        centerpoint ((-358., -38.), (-334., -79.), (-288., -37.), (-322., 3.))
    | 'C', 1 ->
        centerpoint ((-106., 274.), (-54., 264.), (-49., 332.), (-94., 332.))
    | 'C', 2 ->
        centerpoint ((-118., 210.), (-60., 190.), (-54., 264.), (-106., 274.))
    | 'C', 3 ->
        centerpoint ((-130., 149.), (-66., 118.), (-60., 190.), (-118., 210.))
    | 'C', 4 ->
        centerpoint ((-142., 86.), (-71., 45.), (-66., 118.), (-130., 149.))
    | 'C', 5 ->
        centerpoint ((-189., 44.), (-131., 3.), (-71., 45.), (-142., 86.))
    | 'C', 6 ->
        centerpoint ((-237., 2.), (-191., -38.), (-131., 3.), (-189., 44.))
    | 'C', 7 ->
        centerpoint ((-285., -38.), (-251., -79.), (-191., -38.), (-237., 2.))
    | 'C', 8 ->
        centerpoint ((-333., -81.), (-310., -121.), (-252., -80.), (-287., -41.))
    | 'D', 1 -> centerpoint ((-52., 262.), (0., 252.), (0., 333.), (-47., 332.))
    | 'D', 2 -> centerpoint ((-58., 190.), (0., 170.), (0., 252.), (-52., 262.))
    | 'D', 3 -> centerpoint ((-64., 117.), (0., 87.), (0., 170.), (-58., 190.))
    | 'D', 4 -> centerpoint ((-70., 45.), (0., 3.), (0., 87.), (-64., 117.))
    | 'D', 5 -> centerpoint ((-128., 2.), (-71., -36.), (0., 3.), (-70., 45.))
    | 'D', 6 ->
        centerpoint ((-190., -39.), (-143., -79.), (-71., -36.), (-128., 2.))
    | 'D', 7 ->
        centerpoint ((-250., -81.), (-215., -121.), (-143., -79.), (-190., -39.))
    | 'D', 8 ->
        centerpoint
          ((-310., -122.), (-286., -162.), (-215., -121.), (-250., -81.))
        (*Yanjun Section*)
    | 'E', 1 -> centerpoint ((0., 248.), (49., 261.), (43., 326.), (1., 327.))
    | 'E', 2 -> centerpoint ((1., 169.), (55., 187.), (50., 255.), (1., 245.))
    | 'E', 3 -> centerpoint ((0., 86.), (60., 115.), (52., 178.), (1., 161.))
    | 'E', 4 -> centerpoint ((1., 1.), (67., 41.), (62., 109.), (0., 79.))
    | 'E', 9 -> centerpoint ((3., -2.), (69., -40.), (121., -3.), (70., 33.))
    | 'E', 10 ->
        centerpoint ((77., -42.), (141., -81.), (183., -45.), (131., -8.))
    | 'E', 11 ->
        centerpoint ((149., -83.), (214., -121.), (244., -85.), (192., -50.))
    | 'E', 12 ->
        centerpoint ((218., -127.), (286., -164.), (303., -128.), (251., -90.))
    | 'F', 1 -> centerpoint ((54., 261.), (102., 273.), (88., 327.), (48., 327.))
    | 'F', 2 ->
        centerpoint ((60., 190.), (115., 210.), (103., 265.), (55., 255.))
    | 'F', 3 ->
        centerpoint ((66., 117.), (125., 146.), (115., 200.), (61., 183.))
    | 'F', 4 -> centerpoint ((71., 43.), (138., 83.), (128., 141.), (67., 111.))
    | 'F', 9 -> centerpoint ((76., 40.), (128., 1.), (183., 40.), (140., 77.))
    | 'F', 10 ->
        centerpoint ((135., -2.), (190., -40.), (230., -4.), (185., 34.))
    | 'F', 11 ->
        centerpoint ((196., -43.), (250., -80.), (280., -44.), (237., -8.))
    | 'F', 12 ->
        centerpoint ((256., -86.), (308., -121.), (327., -86.), (286., -49.))
    | 'G', 1 ->
        centerpoint ((107., 272.), (156., 279.), (138., 326.), (98., 328.))
    | 'G', 2 ->
        centerpoint ((120., 210.), (172., 228.), (157., 274.), (109., 265.))
    | 'G', 3 ->
        centerpoint ((132., 150.), (189., 177.), (175., 221.), (121., 204.))
    | 'G', 4 ->
        centerpoint ((144., 86.), (212., 124.), (193., 169.), (135., 140.))
    | 'G', 9 -> centerpoint ((149., 81.), (188., 43.), (240., 83.), (212., 116.))
    | 'G', 10 -> centerpoint ((195., 40.), (236., 3.), (281., 40.), (250., 76.))
    | 'G', 11 ->
        centerpoint ((243., -2.), (285., -36.), (314., -4.), (285., 33.))
    | 'G', 12 ->
        centerpoint ((290., -43.), (332., -80.), (352., -43.), (323., -6.))
    | 'H', 1 ->
        centerpoint ((162., 281.), (207., 292.), (188., 325.), (148., 325.))
    | 'H', 2 ->
        centerpoint ((179., 231.), (231., 247.), (212., 284.), (164., 274.))
    | 'H', 3 ->
        centerpoint ((198., 182.), (256., 207.), (235., 238.), (182., 223.))
    | 'H', 4 ->
        centerpoint ((215., 127.), (280., 165.), (258., 200.), (200., 173.))
    | 'H', 9 ->
        centerpoint ((220., 123.), (251., 87.), (303., 123.), (283., 159.))
    | 'H', 10 ->
        centerpoint ((258., 80.), (287., 44.), (329., 79.), (307., 117.))
    | 'H', 11 -> centerpoint ((293., 41.), (322., 7.), (354., 39.), (332., 75.))
    | 'H', 12 ->
        centerpoint ((325., -2.), (356., -39.), (376., -3.), (355., 31.))
        (*** L's row. ***)
    | 'L', 8 ->
        centerpoint
          ((-192., -331.), (-144., -331.), (-217., -290.), (-162., -280.))
    | 'L', 7 ->
        centerpoint
          ((-144., -331.), (-97., -331.), (-162., -280.), (-109., -270.))
    | 'L', 6 ->
        centerpoint ((-97., -331.), (-49., -331.), (-109., -270.), (-54., -260.))
    | 'L', 5 ->
        centerpoint ((-49., -331.), (0., -331.), (-54., -260.), (0., -250.))
    | 'L', 9 ->
        centerpoint ((0., -331.), (49., -331.), (0., -250.), (54., -260.))
    | 'L', 10 ->
        centerpoint ((49., -331.), (97., -331.), (54., -260.), (109., -270.))
    | 'L', 11 ->
        centerpoint ((97., -331.), (144., -331.), (109., -270.), (162., -280.))
    | 'L', 12 ->
        centerpoint ((144., -331.), (192., -331.), (162., -280.), (217., -290.))
        (**** K's row. ****)
    | 'K', 8 ->
        centerpoint
          ((-217., -290.), (-162., -280.), (-240., -250.), (-180., -228.))
    | 'K', 7 ->
        centerpoint
          ((-162., -280.), (-109., -270.), (-180., -228.), (-120., -207.))
    | 'K', 6 ->
        centerpoint
          ((-109., -270.), (-54., -260.), (-120., -207.), (-60., -187.))
    | 'K', 5 ->
        centerpoint ((-54., -260.), (0., -250.), (-60., -187.), (0., -165.))
    | 'K', 9 ->
        centerpoint ((0., -250.), (54., -260.), (0., -165.), (60., -187.))
    | 'K', 10 ->
        centerpoint ((54., -260.), (109., -270.), (60., -187.), (120., -207.))
    | 'K', 11 ->
        centerpoint ((109., -270.), (162., -280.), (120., -207.), (180., -228.))
    | 'K', 12 ->
        centerpoint ((162., -280.), (217., -290.), (180., -228.), (240., -250.))
        (*** J's row. ***)
    | 'J', 8 ->
        centerpoint
          ((-240., -250.), (-180., -228.), (-246., -206.), (-200., -177.))
    | 'J', 7 ->
        centerpoint
          ((-180., -228.), (-120., -207.), (-200., -177.), (-132., -145.))
    | 'J', 6 ->
        centerpoint
          ((-120., -207.), (-60., -187.), (-132., -145.), (-66., -113.))
    | 'J', 5 ->
        centerpoint ((-60., -187.), (0., -165.), (-66., -113.), (0., -83.))
    | 'J', 9 -> centerpoint ((0., -165.), (60., -187.), (0., -83.), (66., -113.))
    | 'J', 10 ->
        centerpoint ((60., -187.), (120., -207.), (66., -113.), (132., -145.))
    | 'J', 11 ->
        centerpoint ((120., -207.), (180., -228.), (132., -145.), (200., -177.))
    | 'J', 12 ->
        centerpoint ((180., -228.), (240., -250.), (200., -177.), (246., -206.))
        (*** I's row. ***)
    | 'I', 8 ->
        centerpoint
          ((-246., -206.), (-200., -177.), (-288., -165.), (-217., -124.))
    | 'I', 7 ->
        centerpoint
          ((-200., -177.), (-132., -145.), (-217., -124.), (-144., -82.))
    | 'I', 6 ->
        centerpoint ((-132., -145.), (-66., -113.), (-144., -82.), (-72., -41.))
    | 'I', 5 -> centerpoint ((-66., -113.), (0., -83.), (-72., -41.), (0., 0.))
    | 'I', 9 -> centerpoint ((0., -83.), (66., -113.), (0., 0.), (72., -41.))
    | 'I', 10 ->
        centerpoint ((66., -113.), (132., -145.), (72., -41.), (144., -82.))
    | 'I', 11 ->
        centerpoint ((132., -145.), (200., -177.), (144., -82.), (217., -124.))
    | 'I', 12 ->
        centerpoint ((200., -177.), (246., -206.), (217., -124.), (288., -165.))
    | _, _ -> (0., 0.)

  let split_first_character str =
    if String.length str > 0 then
      (String.sub str 0 1, String.sub str 1 (String.length str - 1))
    else ("", "")

  let square_to_mouse_position (square : string option) =
    match square with
    | None -> None
    | Some s ->
        let p = split_first_character s in
        if p = ("A", "1") then
          Some ((-214., 294.), (-161., 283.), (-144., 334.), (-191., 334.))
        else None

  let square_to_string (square : (char * int) option) =
    match square with
    | Some (c, i) -> String.make 1 c ^ string_of_int i
    | None -> raise (Failure "Shouldn't be none")
end
