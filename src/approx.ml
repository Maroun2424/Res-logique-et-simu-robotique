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
      (*On  applique translate sur les sommets pour générer le rectangle transformé*)
      let corners_image =
        List.map (fun p -> translate v p) (corners r)
       in
      rectangle_of_list corners_image
  | Rotate (c, alpha) ->
      (* Ici on  applique transform (rotation) *)
      let corners_image =
            List.map (fun p -> transform  t p) (corners r)
            (* On peut avoir un quadrilatère irrégulier et donc La fonction rectangle_of_list nous 
            renvoie le plus petit rectangle aligné avec les axes (rectangle droit) qui englobe tous ces sommets.
            donc on a automatiquement la meilleure sur-approximation alignée sur les axes *)
      in
      rectangle_of_list corners_image  

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
