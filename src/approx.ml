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

  (* Méthode auxiliaire pour toutes les exécutions du programme possible *)
  let rec run_all_rects (prog : program) (rect : rectangle) : rectangle list list =
    match prog with
    | [] -> [[rect]]  (* Une seule exécution avec la position initiale *)
    | Move t :: reste ->
        let new_rect = transform_rect t rect in
        let rest_executions = run_all_rects reste new_rect in
        List.map (fun exec -> rect :: exec) rest_executions
    | Repeat (n, sousProg) :: reste ->
        let rec repeat rect_current n acc =
          if n = 0 then [acc]
          else
            let new_rects = run_all_rects sousProg rect_current in
            let repeated_executions =
              List.concat (List.map (fun new_exec ->
                let last_rect = List.hd (List.rev new_exec) in
                repeat last_rect (n - 1) (acc @ (List.tl new_exec))
              ) new_rects)
            in
            repeated_executions
        in
        let repeated_rects = repeat rect n [rect] in
        List.concat (List.map (fun repeated_execution ->
          let final_rect = List.hd (List.rev repeated_execution) in
          List.map (fun exec -> repeated_execution @ exec) (run_all_rects reste final_rect)
        ) repeated_rects)
    | Either (prog1, prog2) :: reste ->
        let exec1 = run_all_rects (prog1 @ reste) rect in
        let exec2 = run_all_rects (prog2 @ reste) rect in
        exec1 @ exec2

  let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
    let all_executions = run_all_rects prog r in
    List.for_all (fun exec ->
      let last_rect = List.hd (List.rev exec) in
      inclusion last_rect target
    ) all_executions


let rec run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
    match prog with
      | [] -> [i]  (* Cas de base : programme vide *)
      | Move t :: reste ->
        let new_i = transform t i in
        i :: run_polymorphe transform reste new_i

      | Repeat (n, sousProg) :: reste ->
            (* Appelle récursivement run_polymorphe sur le sous-programme *)
            let repeated_states =
              let rec repeat current_i n acc =
                if n = 0 then acc
                else
                let new_states = run_polymorphe transform sousProg current_i in
                let last_state = List.hd (List.rev new_states) in
              repeat last_state (n - 1) (acc @ List.tl new_states)
              in
            repeat i n [i]
            in
            (* Combine les états répétés avec la suite du programme *)
            let without_last = List.rev (List.tl (List.rev repeated_states)) in
            without_last @ run_polymorphe transform reste (List.hd (List.rev repeated_states))
      
      | Either (prog1, prog2) :: reste ->
        (* Choix non déterministe : on exécute l'un des deux sous-programmes *)
        let choix = if Random.bool () then prog1 else prog2 in
        run_polymorphe transform (choix @ reste) i
    
    
    
(* Méthode auxilaire pour calculer le plus petit rectangle qui englobe r1 et r2 *)
let bounding_rect (r1 : rectangle) (r2 : rectangle) : rectangle =
    {
      x_min = min r1.x_min r2.x_min;
      x_max = max r1.x_max r2.x_max;
      y_min = min r1.y_min r2.y_min;
      y_max = max r1.y_max r2.y_max;
    }

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
    match prog with
      | [] -> r
      
      | Move t :: reste ->
        let corners_list = corners r in
        let transformed_corners = List.map (transform t) corners_list in
        let r_new = rectangle_of_list transformed_corners in
        over_approximate reste r_new
    
      | Either (prog1, prog2) :: reste ->
        let r1 = over_approximate prog1 r in
        let r2 = over_approximate prog2 r in
        let r_combined = bounding_rect r1 r2 in
        over_approximate reste r_combined
    
      | Repeat (n, sous_prog) :: reste ->
          (* Calcul de la sur-approximation du sous-programme une seule fois *)
          let r_sub = over_approximate sous_prog r in
          (* On calcule les déplacements minimaux et maximaux *)
          let delta_x_min = r_sub.x_min -. r.x_min in
          let delta_x_max = r_sub.x_max -. r.x_max in
          let delta_y_min = r_sub.y_min -. r.y_min in
          let delta_y_max = r_sub.y_max -. r.y_max in
          (*Onn calcule le rectangle après n répétitions *)
          let r_new = {
            x_min = r.x_min +. (float_of_int n) *. delta_x_min;
            x_max = r.x_max +. (float_of_int n) *. delta_x_max;
            y_min = r.y_min +. (float_of_int n) *. delta_y_min;
            y_max = r.y_max +. (float_of_int n) *. delta_y_max;
          } in
          over_approximate reste r_new

(* Vérifie si une cible est atteinte via la sur-approximation *)
let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  let over_approx = over_approximate prog r in
  inclusion over_approx target
