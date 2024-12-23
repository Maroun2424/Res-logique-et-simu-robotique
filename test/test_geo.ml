open Geo
open Alcotest

(* Définition des testable pour comparer et afficher les types*)

(* Affichage et comparaison*)
(*pretty printers*)

let pp_vector ppf vec =
  Fmt.pf ppf "{%g, %g}" vec.x vec.y

let vector_equal v1 v2 =
  Float.abs (v1.x -. v2.x) < 1e-10 && Float.abs (v1.y -. v2.y) < 1e-10 (* 1e-10 pour gérer les imprécisions ( tolérance )  *)

let testable_vector = testable pp_vector vector_equal

let pp_rectangle ppf r =
  Fmt.pf ppf "{x_min = %g; x_max = %g; y_min = %g; y_max = %g}"
    r.x_min r.x_max r.y_min r.y_max

let rectangle_equal r1 r2 =
  Float.abs (r1.x_min -. r2.x_min) < 1e-10 &&
  Float.abs (r1.x_max -. r2.x_max) < 1e-10 &&
  Float.abs (r1.y_min -. r2.y_min) < 1e-10 &&
  Float.abs (r1.y_max -. r2.y_max) < 1e-10

let testable_rectangle = testable pp_rectangle rectangle_equal

(* Tests unitaires *)

(*translate*)
let test_translate () =
  let tests = [
    ({x = 1.; y = 1.}, {x = 2.5; y = 4.5}, {x = 3.5; y = 5.5});
    ({x = 2.; y = 3.}, {x = 1.; y = 1.}, {x = 3.; y = 4.});
    ({x = -1.; y = -2.}, {x = 0.; y = 0.}, {x = -1.; y = -2.});
    ({x = 5.; y = -3.}, {x = -5.; y = 3.}, {x = 0.; y = 0.})
  ] in
  let rec iter_tests = function
    | [] -> ()
    | (v, p, correct_result) :: rest ->
        check testable_vector "translate" correct_result (translate v p);
        iter_tests rest
  in
  iter_tests tests

  let tests_rad_to_deg = [
    (0.0, 0.0);
    (Float.pi /. 2., 90.0);
    (Float.pi, 180.0);
    (-. Float.pi /. 2., -90.0)
  ]
  
  let tests_deg_to_rad = [
    (0.0, 0.0);
    (90.0, Float.pi /. 2.);
    (180.0, Float.pi);
    (-90.0, -. Float.pi /. 2.)
  ]
  
  (* fonction auxiliaire -> itérer sur les tests *)
  let rec iter_tests_rad_to_deg = function
    | [] -> ()
    | (rad, deg) :: rest ->
        check (float 1e-10) "rad_of_deg" rad (rad_of_deg deg); (* comparaison avec tolérence*)
        iter_tests_rad_to_deg rest
  
  let rec iter_tests_deg_to_rad = function
    | [] -> ()
    | (deg, rad) :: rest ->
        check (float 1e-10) "deg_of_rad" deg (deg_of_rad rad); (* comparaison avec tolérence*)
        iter_tests_deg_to_rad rest
  
  let test_angle_conversion () =
    iter_tests_rad_to_deg tests_rad_to_deg;
    iter_tests_deg_to_rad tests_deg_to_rad


let test_rotate () =
  (* Test fonction rotate *)
  let p = { x = 0.; y = 1. } in
  let c = { x = 0.; y = 0. } in (* centre de rotationn*)
  Alcotest.(check testable_vector)
    "Rotation de 90° autour de l'origine"
    { x = -1.; y = 0. }
    (rotate c 90. p)



let test_in_rectangle () =
  let rect = {x_min = 0.0; x_max = 2.0; y_min = 0.0; y_max = 2.0} in
  let tests = [
    ({x = 1.0; y = 1.0}, true);
    ({x = 0.0; y = 2.0}, true);
    ({x = 3.0; y = 3.0}, false)
  ] in
  let rec iter_tests = function
  | [] -> ()
  | (point, expected) :: rest ->
      check bool "in_rectangle" expected (in_rectangle rect point);
      iter_tests rest
in
iter_tests tests


let test_corners () =
  let rect1 = {x_min = 0.0; x_max = 2.0; y_min = 0.0; y_max = 2.0} in
  let expected1 = [
    {x = 0.0; y = 2.0}; {x = 2.0; y = 2.0};
    {x = 2.0; y = 0.0}; {x = 0.0; y = 0.0}
  ] in
  check (list testable_vector) "corners rect1" expected1 (corners rect1); (* testeur de list de vecteurs , élément par élément*)

  let rect2 = {x_min = 1.0; x_max = 1.0; y_min = 0.0; y_max = 2.0} in
  let expected2 = [
    {x = 1.0; y = 2.0}; {x = 1.0; y = 2.0};
    {x = 1.0; y = 0.0}; {x = 1.0; y = 0.0}
  ] in
  check (list testable_vector) "corners rect2" expected2 (corners rect2)

let test_rectangle_of_list () =
  let tests = [
    ([{x = 0.0; y = 0.0};  {x = 1.0; y = 1.0}; {x = 2.0; y =  2.0}],
      
    {x_min = 0.0; x_max = 2.0 ; y_min = 0.0;  y_max = 2.0 });
    ([{x = -1.0; y  = -1.0}; {x = 0.0; y = 0.0}; {x = 2.0; y = -3.0}],
      {x_min = - 1.0; x_max = 2.0; y_min = -3.0 ; y_max = 0.0})
  ] in
  let rec iter_tests = function
    | [] -> ()
    | (points, expected) :: rest ->
        check testable_rectangle "rectangle_of_list" expected (rectangle_of_list points);
        iter_tests rest
  in
  iter_tests tests


(* Regroupement des tests dans une list *)
let tests = [
  "translate", `Quick, test_translate; (* Quick pour les tests rapides *)
  "rotate", `Quick, test_rotate;
  "angle_conversion", `Quick, test_angle_conversion;
  "in_rectangle", `Quick, test_in_rectangle;
  "corners", `Quick, test_corners;
  "rectangle_of_list", `Quick, test_rectangle_of_list;
]

(* Lancer les tests *)
let () = Alcotest.run "Geo Tests personnels" [
  "Geo functions", tests
]