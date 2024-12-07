open Geo
open Interp

(* Code de la Section 5 du projet. *)

let sample (rect : rectangle) : point =
  let x= Random.float ( rect.x_max -. rect.x_min) +. rect.x_min
  and  y= Random.float  ( rect.y_max -. rect.y_min) +. rect.y_min
  in
  { x; y } (*Génère un point aléatoire à l'intérieur du rectangle *)
  
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  match t with
  | Translate v ->
  (* Applique la translation au rectangle *)
    {  x_min = r.x_min +. v.x;
      x_max = r.x_max +. v.x;
      y_min = r.y_min +. v.y;
      y_max = r.y_max +. v.y}
  | Rotate (c, alpha) ->
  (* Applique la rotation au rectangle et calcule le nouveau rectangle englobant *)
    let rotated_corners = List.map (rotate c alpha) (corners r) in
    rectangle_of_list rotated_corners  

let run_rect (prog : program) (r : rectangle) : rectangle list =
  failwith "À compléter"

let inclusion (r : rectangle) (t : rectangle) : bool =
  failwith "À compléter"

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"

let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  failwith "À compléter"

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
