(* test_interp.ml *)

open Geo
open Interp
open Alcotest
open Fmt

(* pp et testable de test_absint *)
let pp_vector ppf vec =
  Fmt.pf ppf "{%g,%g}" vec.x vec.y

let pp_transformation ppf = function
  | Translate vec ->
      Fmt.pf ppf "TRANS(%a)"
        pp_vector vec
  | Rotate (p, angle) ->
      Fmt.pf ppf "ROT(%a,%g)" 
        pp_vector p
        angle

let rec pp_instruction ppf = function
  | Move op ->
    Fmt.pf ppf "Move %a"
      pp_transformation op
  | Repeat (n, prog) ->
    Fmt.pf ppf "Repeat (%i, %a)"
      n
      pp_program prog
  | Either (prog1, prog2) ->
    Fmt.pf ppf "Either (%a, %a)"
      pp_program prog1
      pp_program prog2
and pp_program ppf =
  Fmt.(brackets @@ list ~sep:(const string "; ") pp_instruction) ppf
    
let float_equal f1 f2 =
  Float.abs (f2 -. f1) < 0.01
let vector_equal vec1 vec2 =
  float_equal vec1.x vec2.x &&
  float_equal vec1.y vec2.y

let testable_vector =
  Alcotest.testable pp_vector vector_equal

let testable_program =
  Alcotest.testable pp_program (=)


(* Fonctions de Test Unitaire *)
(* run *)

let test_run_simple () =
  let prog = [
    Move (Translate {x = 1.0; y = 0.0});
    Move (Translate {x = 0.0; y = 1.0});
  ] in
  let start_point = {x = 0.0; y = 0.0} in
  let expected_positions = [
    {x = 0.0; y = 0.0};
    {x = 1.0; y = 0.0};
    {x = 1.0; y = 1.0};
  ] in
  check (Alcotest.list testable_vector) "run simple program" expected_positions (Interp.run prog start_point)

let test_run_with_repeat () =
  let prog = [
    Repeat (3, [
      Move (Translate {x = 1.0; y = 1.0});
    ]);
    Move (Translate {x = -3.0; y = -3.0});
  ] in
  let start_point = {x = 0.0; y = 0.0} in
  let expected_positions = [
    {x = 0.0; y = 0.0};
    {x = 1.0; y = 1.0};
    {x = 2.0; y = 2.0};
    {x = 3.0; y = 3.0};
    {x = 0.0; y = 0.0};
  ] in
  check (Alcotest.list testable_vector) "run with repeat" expected_positions (Interp.run prog start_point)

let test_run_with_either () =
  let prog = [
    Either (
      [Move (Translate {x = 1.0; y = 0.0})],
      [Move (Translate {x = 0.0; y = 1.0})]
    );
    Move (Translate {x = 1.0; y = 1.0});
  ] in
  let start_point = {x = 0.0; y = 0.0} in
  (* Le résultat dépend de Random.bool (), donc on teste les deux possibilités *)
  let expected_positions1 = [
    {x = 0.0; y = 0.0};
    {x = 1.0; y = 0.0};
    {x = 2.0; y = 1.0};
  ] in
  let expected_positions2 = [
    {x = 0.0; y = 0.0};
    {x = 0.0; y = 1.0};
    {x = 1.0; y = 2.0};
  ] in
  let result = Interp.run prog start_point in
  if List.length result = 3 then
    if List.for_all2 vector_equal result expected_positions1 || List.for_all2 vector_equal result expected_positions2 then
      ()
    else
      failwith "unexpected results"
  else
    failwith "incorrect number of positions"

(* all_choices *)


let test_all_choices_simple () =
  let prog = [
    Move (Translate {x = 1.0; y = 0.0});
    Move (Translate {x = 0.0; y = 1.0});
  ] in
  let expected_choices = [
    [
      Move (Translate {x = 1.0; y = 0.0});
      Move (Translate {x = 0.0; y = 1.0});
    ]
  ] in
  check (Alcotest.list testable_program) "all_choices simple program" expected_choices (all_choices prog)

let test_all_choices_with_either () =
  let prog = [
    Move (Translate {x = 1.0; y = 0.0});
    Either (
      [Move (Translate {x = 0.0; y = 1.0})],
      [Move (Translate {x = -1.0; y = 0.0})]
    );
  ] in
  let expected_choices = [
    [
      Move (Translate {x = 1.0; y = 0.0});
      Move (Translate {x = 0.0; y = 1.0});
    ];
    [
      Move (Translate {x = 1.0; y = 0.0});
      Move (Translate {x = -1.0; y = 0.0});
    ];
  ] in
  check (Alcotest.list testable_program) "all_choices with either" expected_choices (all_choices prog)

let test_all_choices_with_repeat () =
  let prog = [
    Repeat (2, [
      Move (Translate {x = 1.0; y = 1.0})
    ]);
  ] in
  let expected_choices = [
    [
      Move (Translate {x = 1.0; y = 1.0});
      Move (Translate {x = 1.0; y = 1.0});
    ]
  ] in
  check (Alcotest.list testable_program) "all_choices with repeat" expected_choices (all_choices prog)

let test_all_choices_multiple_either () =
  let prog = [
    Either (
      [Move (Translate {x = 1.0; y = 0.0})],
      [Move (Translate {x = 0.0; y = 1.0})]
    );
    Either (
      [Move (Translate {x = -1.0; y = 0.0})],
      [Move (Translate {x = 0.0; y = -1.0})]
    );
  ] in
  let expected_choices = [
    [
      Move (Translate {x = 1.0; y = 0.0});
      Move (Translate {x = -1.0; y = 0.0});
    ];
    [
      Move (Translate {x = 1.0; y = 0.0});
      Move (Translate {x = 0.0; y = -1.0});
    ];
    [
      Move (Translate {x = 0.0; y = 1.0});
      Move (Translate {x = -1.0; y = 0.0});
    ];
    [
      Move (Translate {x = 0.0; y = 1.0});
      Move (Translate {x = 0.0; y = -1.0});
    ];
  ] in
  check (Alcotest.list testable_program) "all_choices multiple either" expected_choices (all_choices prog)

(* run_det*)

let test_run_det_simple () =
  let prog = [
    Move (Translate {x = 1.0; y = 0.0});
    Move (Translate {x = 0.0; y = 1.0});
  ] in
  let start_point = {x = 0.0; y = 0.0} in
  let expected_positions = [
    {x = 0.0; y = 0.0};
    {x = 1.0; y = 0.0};
    {x = 1.0; y = 1.0};
  ] in
  check (Alcotest.list testable_vector) "run_det simple program" expected_positions (run_det prog start_point)

let test_run_det_with_repeat () =
  let prog = [
    Repeat (3, [
      Move (Translate {x = 1.0; y = 1.0});
    ]);
    Move (Translate {x = -3.0; y = -3.0});
  ] in
  let start_point = {x = 0.0; y = 0.0} in
  let expected_positions = [
    {x = 0.0; y = 0.0};
    {x = 1.0; y = 1.0};
    {x = 2.0; y = 2.0};
    {x = 3.0; y = 3.0};
    {x = 0.0; y = 0.0};
  ] in
  check (Alcotest.list testable_vector) "run_det with repeat" expected_positions (run_det prog start_point)

(* target_reached *)

let test_target_reached_true () =
  let prog = [
    Move (Translate {x = 1.0; y = 1.0});
    Move (Translate {x = 1.0; y = 1.0});
  ] in
  let start_p = {x = 0.0; y= 0.0 } in
  let target_rect = {x_min = 2.0; y_min = 2.0; x_max = 3.0; y_max = 3.0} in
  (* Après deux translations de (1,1), on atteint {2,2; 3,3} par ex *)
  let result = Interp.target_reached prog start_p target_rect in
  Alcotest.check Alcotest.bool "target_reached true" true result

let test_target_reached_false () =
  let prog = [
    Move (Translate {x = 1.0; y = 1.0});
    Move (Translate {x = 1.0; y = 1.0});
  ] in
  let start_p = {x = 0.0; y = 0.0} in
  let target_rect = {x_min = 3.0; y_min = 3.0; x_max = 4.0; y_max = 4.0} in
  (* Après deux translations de (1,1), on atteint {2,2; 3,3} par ex ce qui n'est pas entièrement dans {3,3 4,4} *)
  let result = target_reached prog start_p target_rect in
  Alcotest.check Alcotest.bool "target_reached false" false result

(* Les Tests *)

let tests = [
  "run simple program", `Quick, test_run_simple;
  "run with repeat", `Quick, test_run_with_repeat;
  "run with either", `Quick, test_run_with_either;

  "all_choices simple program", `Quick, test_all_choices_simple;
  "all_choices with either", `Quick, test_all_choices_with_either;
  "all_choices with repeat", `Quick, test_all_choices_with_repeat;
  "all_choices multiple either", `Quick, test_all_choices_multiple_either;

  "run_det simple program", `Quick, test_run_det_simple;
  "run_det with repeat", `Quick, test_run_det_with_repeat;

  "target_reached true", `Quick, test_target_reached_true;
  "target_reached false", `Quick, test_target_reached_false;
]

let () =
  Alcotest.run "Interp Tests" [
    "Interp functions", tests
  ]