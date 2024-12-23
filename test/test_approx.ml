(* test_approx.ml *)

open Geo
open Interp
open Approx
open Alcotest
open Fmt


(* pp et testable inspirés de test_absint.ml*)

let pp_rectangle ppf r =
  Fmt.pf ppf "{%g,%g,%g,%g}" r.x_min r.x_max r.y_min r.y_max

    
let float_equal f1 f2 =
  Float.abs (f2 -. f1) < 0.01

let rectangle_equal r1 r2 =
  float_equal r1.x_min r2.x_min &&
  float_equal r1.x_max r2.x_max &&
  float_equal r1.y_min r2.y_min &&
  float_equal r1.y_max r2.y_max


let testable_rectangle =
  Alcotest.testable pp_rectangle rectangle_equal

(* Pretty Printer pour les listes de rectangles *)
let pp_rectangle_list ppf lst =
  pf ppf "[%a]" (list ~sep:(const string "; ") pp_rectangle) lst

let rectangle_list_equal l1 l2 =
  List.length l1 = List.length l2 &&
  List.for_all2 rectangle_equal l1 l2

let testable_rectangle_list =
  testable pp_rectangle_list rectangle_list_equal




(* transform_rect *)

let test_transform_translate () =
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let translation = Translate {x = 2.0; y = 3.0} in
  let expected = {x_min = 2.0; y_min = 3.0; x_max = 3.0; y_max = 4.0} in
  let result = transform_rect translation rect in
  Alcotest.check testable_rectangle "translate rectangle" expected result

let test_transform_rotate () =
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let rotation = Rotate ({x = 0.0; y = 0.0}, 0. ) in
  let expected = {x_min = 0. ; y_min = 0. ; x_max = 1.; y_max = 1. } in
  let result = transform_rect rotation rect in
  Alcotest.check testable_rectangle "rotate rectangle" expected result

(* inclusion *)

let test_inclusion_true () =
  let r = {x_min = 1.0; y_min = 1.0; x_max = 2.0; y_max = 2.0} in
  let t = {x_min = 0.0; y_min = 0.0; x_max = 3.0; y_max = 3.0} in
  let result = inclusion r t in
  Alcotest.check Alcotest.bool "inclusion true" true result

let test_inclusion_false () =
  let r = {x_min = 1.0; y_min = 1.0; x_max = 4.0; y_max = 4.0} in
  let t = {x_min = 0.0; y_min = 0.0; x_max = 3.0; y_max = 3.0} in
  let result = inclusion r t in
  Alcotest.check Alcotest.bool "inclusion false" false result

(* run_rect *)

let test_run_rect_simple () =
  let prog = [
    Move (Translate {x = 1.0; y = 1.0});
    Move (Translate {x = 2.0; y = 2.0});
  ] in
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let expected = [
    {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0};
    {x_min = 1.0; y_min = 1.0; x_max = 2.0; y_max = 2.0};
    {x_min = 3.0; y_min = 3.0; x_max = 4.0; y_max = 4.0};
  ] in
  let result = run_rect prog rect in
  Alcotest.check testable_rectangle_list "run_rect simple" expected result

(* target_reached_rect *)

let test_target_reached_rect_true () =
  let prog = [
    Move (Translate {x = 1.0; y = 1.0});
    Move (Translate {x = 1.0; y = 1.0});
  ] in
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let target = {x_min = 2.0; y_min = 2.0; x_max = 3.0; y_max = 3.0} in
  let result = target_reached_rect prog rect target in
  Alcotest.check Alcotest.bool "target_reached_rect true" true result

let test_target_reached_rect_false () =
  let prog = [
    Move (Translate {x = 1.0; y = 1.0});
    Move (Translate {x = 1.0; y = 1.0});
  ] in
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let target = {x_min = 3.0; y_min = 3.0; x_max = 4.0; y_max = 4.0} in
  let result = target_reached_rect prog rect target in
  Alcotest.check Alcotest.bool "target_reached_rect false" false result

(*over_approximate*)

let test_over_approximate_simple () =
  let prog = [
    Move (Translate {x = 1.0; y = 1.0});
  ] in
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let expected = {x_min = 1.0; y_min = 1.0; x_max = 2.0; y_max = 2.0} in
  let result = over_approximate prog rect in
  Alcotest.check testable_rectangle "over_approximate simple" expected result

let test_over_approximate_complex () =
  let prog = [
    Move (Translate {x = 1.0; y = 0.0});
    Repeat (2, [
      Move (Translate {x = 0.0; y = 1.0});
      Move (Translate {x = 1.0; y = 1.0});
    ]);
    Either (
      [Move (Translate {x = -1.0; y = 0.0})],
      [Move (Translate {x = 0.0; y = -1.0})]
    );
  ] in
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let expected = {x_min = 2.0; y_min = 3.0; x_max = 4.0; y_max = 5.0} in
  let result = over_approximate prog rect in
  Alcotest.check testable_rectangle "over_approximate complex" expected result

(*feasible_target_reached *)

let test_feasible_target_reached_true () =
  let prog = [
    Move (Translate {x = 1.0; y = 1.0});
    Move (Translate {x = 1.0; y = 1.0});
  ] in
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let target = {x_min = 2.0; y_min = 2.0; x_max = 4.0; y_max = 4.0} in
  let result = feasible_target_reached prog rect target in
  Alcotest.check Alcotest.bool "feasible_target_reached true" true result

let test_feasible_target_reached_false () =
  let prog = [
    Move (Translate {x = 1.0; y = 1.0});
    Move (Translate {x = 1.0; y = 1.0});
  ] in
  let rect = {x_min = 0.0; y_min = 0.0; x_max = 1.0; y_max = 1.0} in
  let target = {x_min = 3.0; y_min = 3.0; x_max = 4.0; y_max = 4.0} in
  let result = feasible_target_reached prog rect target in
  Alcotest.check Alcotest.bool "feasible_target_reached false" false result




let tests = [
  "transform_translate", `Quick, test_transform_translate;
  "transform_rotate", `Quick, test_transform_rotate;

  "inclusion true", `Quick, test_inclusion_true;
  "inclusion false", `Quick, test_inclusion_false;

  "run_rect simple", `Quick, test_run_rect_simple;

  "target_reached_rect true", `Quick, test_target_reached_rect_true;
  "target_reached_rect false", `Quick, test_target_reached_rect_false;

  "feasible_target_reached true", `Quick, test_feasible_target_reached_true;
  "feasible_target_reached false", `Quick, test_feasible_target_reached_false;

  "over_approximate simple", `Quick, test_over_approximate_simple;
  "over_approximate complex", `Quick, test_over_approximate_complex;
]


let () =
  Alcotest.run "Approx Tests" [
    "Approx functions", tests
  ]