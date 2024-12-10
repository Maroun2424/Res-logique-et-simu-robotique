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

let rec run_rect (prog : program) (rect : rectangle) : rectangle list =
  match prog with
    | [] -> [rect]  (* Cas de base; programme vide.\   *)
    | Move t :: reste ->
        let new_rect = transform_rect t rect in
        rect :: run_rect reste new_rect  (* Ajout du rectangle transformé à *)
    | Repeat (n, sousProg) :: reste -> (* Cas repeat : TODO a ameliorer  et redundant code  *)
        let rec repeat rect_current n acc =
              if n = 0 then acc
              else 
                let new_rects = run_rect sousProg rect_current in (* tout les rectangles obtenus d'une execution de sousprog *)
                let last_rect = List.hd (List.rev new_rects) in (*on prend le dernier rectangle en inversant la liste *)
                repeat last_rect (n - 1) (acc @ List.tl new_rects) (* on continue d'executer le sousprog*)
            in 
            let repeated_rects = repeat rect n [rect] in
            let without_last = List.rev (List.tl (List.rev repeated_rects)) in (* a revoir *)
            without_last @ run_rect reste (List.hd (List.rev repeated_rects))
    | Either (prog1, prog2) :: reste -> (* cas either similaire comme run*)
        let choix= if  Random.bool ()  then prog1 else prog2  in
        run_rect (choix @ reste) rect

let inclusion (r : rectangle) (t : rectangle) : bool =
  r.x_min >= t.x_min && r.x_max <= t.x_max && r.y_min >= t.y_min && r.y_max <= t.y_max (*renvoie true si le premier rectangle estcontenu entièrement dans le second, false sinon*)

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"

let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  failwith "À compléter"

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
