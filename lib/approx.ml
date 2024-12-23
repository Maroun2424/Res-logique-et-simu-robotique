open Geo
open Interp

(* Code de la Section 5 du projet. *)

let sample (rect : rectangle) : point =
  let x= Random.float ( rect.x_max -. rect.x_min) +. rect.x_min
  and  y= Random.float  ( rect.y_max -. rect.y_min) +. rect.y_min
  in
  { x; y } (*Génère un point aléatoire à l'intérieur du rectangle *)
  
(* Cette méthode applique `transformation` à un rectangle r et renvoie le plus petit rectangle 
    contenant les sommets du rectangle résultat *)
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  match t with
  | Translate v ->
      (*On  applique translate sur les sommets pour générer le rectangle transformé*)
      let corners_image = List.map (fun p -> translate v p) (corners r)
      in
      rectangle_of_list corners_image
  | Rotate (c, alpha) ->
      (* Ici on  applique transform (rotation) *)
      let corners_image = List.map (fun p -> transform  t p) (corners r)
            (* On peut avoir un quadrilatère irrégulier et donc La fonction rectangle_of_list nous 
            renvoie le plus petit rectangle aligné avec les axes (rectangle droit) qui englobe tous ces sommets.
            donc on a automatiquement la meilleure sur-approximation alignée sur les axes *)
      in
      rectangle_of_list corners_image

(* Qst 5.5 en avance pour factoriser le code et la réutiliser dans run_rect *)
(* Exécute un programme sur un état initial en retournant la liste des états visités successifs *)
let rec run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  match prog with
    | [] -> [i]  (* Cas de base : programme vide *)
    | Move t :: reste ->
      let new_i = transform t i in
      i :: run_polymorphe transform reste new_i

    | Repeat (n, sousProg) :: reste ->
      (*  récursivement run_polymorphe sur le sous-programme *)
      let repeated_states =
        let rec repeat current_i nb acc =
          if nb = 0 then acc
          else
            let new_images = run_polymorphe transform sousProg current_i in 
            let last_state = List.hd (List.rev new_images) in
            repeat last_state (nb - 1) (acc @ List.tl new_images)
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
    

(* Exécute un prog sur un rectangle, en retournant la liste des rectangles visités ( similar to run ) 
    Pour factoriser le code on réutilise la méthode run_polymorphe *)
let run_rect (prog : program) (rect : rectangle) : rectangle list =
  run_polymorphe transform_rect prog rect

(* Code run_rect initial :
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
*)

let inclusion (r : rectangle) (t : rectangle) : bool =
  r.x_min >= t.x_min && r.x_max <= t.x_max && r.y_min >= t.y_min && r.y_max <= t.y_max (*renvoie true si le premier rectangle estcontenu entièrement dans le second, false sinon*)

(* Méthode en PLUS pour toutes les exécutions du programme possibles
    Code refactor : on réutilise all_choices de interp.ml pour avoir tous les programmes déterministes possibles 
    et puis on applique run_rect à chaque *)
let run_all_rects (prog : program) (rect : rectangle) : rectangle list list =
  let deterministic_progs = all_choices prog in
  List.map (fun det_prog -> run_rect det_prog rect) deterministic_progs

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
    let all_executions = run_all_rects prog r in
    List.for_all (fun exec ->
      let last_rect = List.hd (List.rev exec) in
      inclusion last_rect target
    ) all_executions

    
let rec over_approximate (prog : program) (r : rectangle) : rectangle =
    match prog with
      | [] -> r
      
      | Move t :: reste -> (* applique move *)
        let corners_list = corners r in
        let transformed_corners = List.map (transform t) corners_list in
        let r_new = rectangle_of_list transformed_corners in
        over_approximate reste r_new
    
      | Either (prog1, prog2) :: reste ->
        let r1 = over_approximate prog1 r in
        let r2 = over_approximate prog2 r in
        (* geo.ml *)
        let r_combined = rectangle_of_list (corners r1 @ corners r2) in
        over_approximate reste r_combined
    
      | Repeat (n, sous_prog) :: reste ->
          (* Calcul de la sur-approximation du sous-programme une seule fois *)
          let r_sub = over_approximate sous_prog r in
          (* On calcule les déplacements minimaux et maximaux *)
          let delta_x_min = r_sub.x_min -. r.x_min in
          let delta_x_max = r_sub.x_max -. r.x_max in
          let delta_y_min = r_sub.y_min -. r.y_min in
          let delta_y_max = r_sub.y_max -. r.y_max in
          (*Onn calcule le rectangle après n répétitions en extrapolant les déplacements *)
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
