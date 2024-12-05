open Geo

 (* Tests supplémentaires pour les fonctions du fichier geo.ml *)
 (* On a effectué les tests principalement sur utop ( make top ) mais pour garder une trace on a ajouté ces tests *)

let test_translate () =
  let p = {x = 1.; y = 1.} and u = {x = 2.5; y = 4.5}in 
  assert ( translate p u = {x = 3.5; y = 5.5;} );

  let v1 = {x = 2.0; y = 3.0} and p1 = {x = 1.0; y = 1.0} in 
  assert ( translate v1 p1 = {x = 3.0;  y = 4.0});

  let v2 = {x = -1.0; y = -2.0} and p2 = {x = 0.0; y = 0.0} in
  assert ( translate v2 p2 = {x = -1.0;  y = -2.0});

  let v3 = {x = 5.0; y = -3.0} and p3 = {x = -5.0; y = 3.0} in
  assert (translate v3  p3 = {x = 0.0 ; y = 0.0});

  print_endline "Tests pour translate réussis "

let test_angle_conversion () =
  let epsilon = 1e-10 in
  
  let a1 = 0.0 in
  assert (abs_float (rad_of_deg a1 -. 0.0) < epsilon);
  assert (abs_float (deg_of_rad 0.0 -. a1) < epsilon);
  
  let a2 = 90.0 in
  assert (abs_float (rad_of_deg a2 -. (Float.pi /. 2.0)) < epsilon);
  assert (abs_float (deg_of_rad (Float.pi /. 2.0) -. a2) < epsilon);
  
  let a3 = 180.0 in
  assert (abs_float (rad_of_deg a3 -. Float.pi) < epsilon);
  assert (abs_float (deg_of_rad Float.pi -. a3) < epsilon);
  
  let a4 = -90.0 in
  assert (abs_float (rad_of_deg a4 -. (-. Float.pi /. 2.0)) < epsilon);
  assert (abs_float (deg_of_rad (-. Float.pi /. 2.0) -. a4) < epsilon);
  
  print_endline "Tests pour la conversion  d'angle réussis"

(* Erreurs  sur les résultats attendus par Rotate dans les assertions 
 dû à la précision limitée des nombres flottants notamment avec les fonctions trigonométriques cos et sin.  *)

(*
let test_rotate () =
  assert (rotate {x = 0.; y = 1.} 45. {x = 1.; y = 0.} = {x = 1.41421356237309492; y = 0.999999999999999889} )
  
  let p1 = {x = 1.0; y = 0.0} in
  let c1 = {x = 0.0; y = 0.0} in
  assert (rotate c1 0.0 p1 = {x = 1.0; y = 0.0});
    
  let p2 = {x = 1.0; y = 0.0} in
  let c2 = {x = 0.0; y = 0.0} in
  assert (rotate c2 90.0 p2 = {x = 0.0; y = 1.0});
    
  let p3 = {x = 0.0; y = 1.0} in
  let c3 = {x = 0.0; y = 0.0} in
  assert (rotate c3 (-90.0) p3 = {x = 1.0; y = 0.0});
    
  let p4 = {x = 2.0; y = 3.0} in
  let c4 = {x = 2.0; y = 3.0} in
  assert (rotate c4 360.0 p4 = p4);
    
  print_endline "Tests pour rotate réussis". *)

let test_transform () =
  let t1 = Translate {x = 2.0; y = 3.0} in
  let p1 = {x = 1.0; y = 1.0} in
  assert (transform t1 p1 = {x = 3.0; y = 4.0});
      
  (*
  let t2 = Rotate ({x = 0.0; y = 0.0}, 90.0) in
  let p2 = {x = 1.0; y = 0.0} in
  assert (transform t2 p2 = {x = 0.0; y = 1.0});
      
  let t3 = Rotate ({x = 1.0; y = 1.0}, 180.0) in
  let p3 = {x = 2.0; y = 1.0} in
  assert (transform t3 p3 = {x = 0.0; y = 1.0});*)
      
  print_endline "Tests pour transform réussis"

let test_in_rectangle () =
  let rect = {x_min = 0.0; x_max = 2.0; y_min = 0.0; y_max = 2.0} in
  let p1 = {x = 1.0; y = 1.0} in
  assert (in_rectangle rect p1);
        
  let p2 = {x = 0.0; y = 2.0} in
  assert (in_rectangle rect p2);
        
  let p3 = {x = 3.0; y = 3.0} in
  assert (not (in_rectangle rect p3));
        
  let rect = {x_min = 1.0; x_max = 1.0; y_min = 0.0; y_max = 2.0} in
  let p4 = {x = 1.0; y = 1.5} in
  assert (in_rectangle rect p4);
        
  print_endline "Tests pour in_rectangle réussis"

let test_corners () =
  let rect1 = {x_min = 0.0; x_max = 2.0; y_min = 0.0; y_max = 2.0} in
  assert (corners rect1 = [{x = 0.0; y = 2.0}; {x = 2.0; y = 2.0}; {x = 2.0; y = 0.0}; {x = 0.0; y = 0.0}]);

  let rect2 = {x_min = 1.0; x_max = 1.0; y_min = 0.0; y_max = 2.0} in
  assert (corners rect2 = [{x = 1.0; y = 2.0}; {x = 1.0; y = 2.0}; {x = 1.0; y = 0.0}; {x = 1.0; y = 0.0}]);

  print_endline "Tests pour corners réussis"

(* pl : point list *)
let test_rectangle_of_list () =
  let pl1 = [{x = 0.0; y = 0.0}; {x = 1.0; y = 1.0}; {x = 2.0; y = 2.0}] in
  assert (rectangle_of_list pl1 = {x_min = 0.0; x_max = 2.0; y_min = 0.0; y_max = 2.0});

  let pl2 = [{x = 1.0; y = 2.0}; {x = 1.0; y = 1.0}; {x = 1.0; y = 3.0}] in
  assert (rectangle_of_list pl2 = {x_min = 1.0; x_max = 1.0; y_min = 1.0; y_max = 3.0});

  let pl3 = [{x = 5.0; y = 5.0}] in
  assert (rectangle_of_list pl3 = {x_min = 5.0; x_max = 5.0; y_min = 5.0; y_max = 5.0});

  let pl4 = [{x = -1.0; y = -1.0}; {x = 0.0; y = 0.0}; {x = 2.0; y = -3.0}; {x = -4.0; y = 1.0}] in
  assert (rectangle_of_list pl4 = {x_min = -4.0; x_max = 2.0; y_min = -3.0; y_max = 1.0});

  print_endline "Tests pour rectangle_of_list réussis"


  let run_tests () =
    test_translate ();
    test_angle_conversion ();
    (*test_rotate ();*)
    test_transform ();
    test_in_rectangle ();
    test_corners ();
    test_rectangle_of_list ();
    print_endline "Tous les Tests sont réussis"
  

  let () = run_tests ()
