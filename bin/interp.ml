open Graphics
open Pf5
open Geo
open Interp
open Approx

(* Gestion des options en ligne de commande *)
type config = {
  abs_rectangle : rectangle option; (* option -abs Xmin Ymin Xmax Ymax *)
  display_points : bool;             (* option -cr, affiche points *)
  background_color : color option;   (* option -bc r g b *)
  foreground_color : color option;  (* option -fc r g b *)
  rectangle_color : color option;  (* option -rc r g b *)
  point_color : color option;    (* option -pc r g b *)
  width : int;                  (* option -size W H *)
  height : int;
  prog_number : int;                 (* programme 1,2,3.. *)
  print_code : bool;                 (*option -print *)
}

(* Configuration par défaut si aucune option n'est fourniee*)
let default_config = {
  abs_rectangle = None;
  display_points = true;                      (* On affiche les points par défaut *)
  background_color = Some (rgb 245 245 220); (* Beige par défaut *)
  foreground_color = Some black;
  rectangle_color = Some blue;
  point_color = Some red;
  width = 800;
  height = 800;
  prog_number = 2;
  print_code = false;                        (* Par défaut, pas d'affichage terminal *)
}

(* Analyse des arguments de la ligne de commande *)
let parse_args () =
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse args config =
    match args with
    | [] -> config

    (* Rectangle d'approximation *)
    | "-abs" :: x_min :: y_min :: x_max :: y_max :: rest ->
        let rect = {
          x_min = float_of_string x_min;
          y_min = float_of_string y_min;
          x_max = float_of_string x_max;
          y_max = float_of_string y_max;
        } in
        parse rest { config with abs_rectangle = Some rect }

    (* Affichage des points*)
    | "-cr" :: rest ->
        parse rest { config with display_points = true }

    (* Couleur de fond *)
    | "-bc" :: r :: g :: b :: rest ->
        let color = rgb (int_of_string r) (int_of_string g) (int_of_string b) in
        parse rest { config with background_color = Some color }

    (* Couleur de l'avant-plan (pas forcément utilisé) *)
    | "-fc" :: r :: g :: b :: rest ->
        let color = rgb (int_of_string r) (int_of_string g) (int_of_string b) in
        parse rest { config with foreground_color = Some color }

    (* Couleur du rectangle -abs *)
    | "-rc" :: r :: g :: b :: rest ->
        let color = rgb (int_of_string r) (int_of_string g) (int_of_string b) in
        parse rest { config with rectangle_color = Some color }

    (* Couleur du point/robot *)
    | "-pc" :: r :: g :: b :: rest ->
        let color = rgb (int_of_string r) (int_of_string g) (int_of_string b) in
        parse rest { config with point_color = Some color }

    (* Taille de la fenêtre *)
    | "-size" :: w :: h :: rest ->
        parse rest { config with width = int_of_string w; height = int_of_string h }

    (*print -> on affiche le code dans le terminal à chaque pas *)
    | "-print" :: rest ->
        parse rest { config with print_code = true }

    (* Choix du program *)
    | prog :: rest when prog = "1" || prog = "2" || prog = "3" || prog = "4" ->
        parse rest { config with prog_number = int_of_string prog }

    (* Erreur : argument non reconnu *)
    | arg :: _ -> failwith ("Argument inconnu ou mal forme : " ^ arg)
  in
  parse args default_config

(* Dessin d'un rectangle (utilisé si -abs est spécifié) *)
let draw_rectangle (rect : rectangle) color scaling_factor =
  set_color color;
  let x = int_of_float (rect.x_min *. scaling_factor) in
  let y = int_of_float (rect.y_min *. scaling_factor) in
  let w = int_of_float ((rect.x_max -. rect.x_min) *. scaling_factor) in
  let h = int_of_float ((rect.y_max -. rect.y_min) *. scaling_factor) in
  draw_rect x y w h

(* Axe gradué*)
let draw_axes width height =
  let step_x = 100 in  (* pour l'axe X *)
  let step_y = 100 in   (*pour l'axe Y *)
  set_color black;

  (* Axe X *)
  moveto 0 (height / 2);
  lineto width (height / 2);

  (* Axe Y *)
  moveto (width / 2) 0;
  lineto (width / 2) height;

  (* Graduation sur l'axe X *)
  let rec draw_ticks_x x =
    if x > width then ()
    else begin
      moveto x (height / 2 - 5);
      lineto x (height / 2 + 5);
      moveto x (height / 2 + 10);
      draw_string (string_of_int (x - width / 2));
      draw_ticks_x (x + step_x)
    end
  in
  (* Graduation sur l'axe Y *)
  let rec draw_ticks_y y =
    if y > height then ()
    else begin
      moveto (width / 2 - 5) y;
      lineto (width / 2 + 5) y;
      moveto (width / 2 + 10) y;
      draw_string (string_of_int (y - height / 2));
      draw_ticks_y (y + step_y)
    end
  in
  draw_ticks_x 0;
  draw_ticks_y 0

(* Convertir une instruction en string (pour l'afficher dans le stdout avec -print)*)
let rec string_of_instruction = function
  | Move (Translate v) ->
      Printf.sprintf "Move(Translate(%.2f, %.2f))" v.x v.y
  | Move (Rotate (p, a)) ->
      Printf.sprintf "Move(Rotate((%.2f, %.2f), %.2f))" p.x p.y a
  | Repeat (n, subprog) ->
      Printf.sprintf "Repeat(%d, [%s])" n (string_of_program subprog)
  | Either (progA, progB) ->
      Printf.sprintf "Either([%s], [%s])"
        (string_of_program progA) (string_of_program progB)

and string_of_program program =
  String.concat "; " (List.map string_of_instruction program)

(* Transforme un point p en coordonnées pixel, centrées. *)
let transform_point (p : point) width height scaling_factor =
  let x = int_of_float (p.x *. scaling_factor) + (width / 2) in
  let y = int_of_float (p.y *. scaling_factor) + (height / 2) in
  (x, y)

(*Programmes predefinis *)
let program1 = [
  (* Dessiner un carré avec des mouvements *)
  Repeat (150, [Move (Translate {x = 1.0; y = 0.0})]);  (* Ligne droite vers la droite *)
  Repeat (100, [Move (Translate {x = 0.0; y = 1.0})]);  (* Ligne droite vers le haut *)
  Repeat (100, [Move (Translate {x = -1.0; y = 0.0})]); (* Ligne droite vers la gauche *)
  Repeat (100, [Move (Translate {x = 0.0; y = -1.0})]); (* Ligne droite vers le bas *)

  Repeat (200, [Move (Translate {x = -1.0; y = 0.0})]); 
  Repeat (100, [Move (Translate {x = 0.0; y = 1.0})]);  
  Repeat (100, [Move (Translate {x = 1.0; y = 0.0})]);  
  Repeat (100, [Move (Translate {x = 0.0; y = -1.0})]);
  Repeat (50, [Move (Translate {x = 1.0; y = 0.0})]); 

  (* Simuler un rectangle "rempli" en diagonale *)
  Repeat (80, [ 
    Move (Translate {x = -1.0; y = -1.0}); (* Diagonale vers le bas gauche *)
    Repeat (80, [Move (Translate {x = 1.0; y = -1.0})]); (* Ligne diagonale descendante *)
    Move (Translate {x = -80.0; y = 80.0}) ;
  ]);

]

let program2 = [
  Move (Translate {x = 0.0; y = -100.0});

  Repeat (36, [
    Move (Translate {x = 10.0; y = 0.0});  
    Move (Rotate ({x = 0.0; y = 0.0}, 10.0))
  ]);

  Move (Translate {x = 0.0; y = 100.0});

  Move (Translate {x = -30.0; y = 50.0});  
  Repeat (12, [
    Move (Translate {x = 5.0; y = 0.0});  
    Move (Rotate ({x = -30.0; y = 50.0}, 30.0))
  ]);

  Move (Translate {x = 60.0; y = 0.0});
  Repeat (12, [
    Move (Translate {x = 5.0; y = 0.0});
    Move (Rotate ({x = 30.0; y = 50.0}, 30.0))
  ]);

  Move (Translate {x = -60.0; y = 0.0});

  Move (Translate {x = 0.0; y = 0.0});
  Move (Rotate ({x = 0.0; y = 0.0}, 90.0));

  Repeat (8, [
    Move (Translate {x = 4.0; y = 0.0});
    Move (Rotate ({x = 0.0; y = -20.0}, 15.0))
  ])
]

let program3 = [
  Repeat (100, [Move (Translate {x = 1.0; y = 0.0})]);
  Repeat (100, [ Move (Translate {x = 0.0; y = 1.0}) ]);
  Repeat (50, [ Move (Translate {x = 1.0; y = 0.0}) ]);
  Repeat (50, [ Move (Translate {x = 0.0; y = 1.0}) ]);
  Repeat (300, [ Move (Translate {x = -1.0; y = 0.0}) ]);
  Repeat (50, [ Move (Translate {x = 0.0; y = -1.0}) ]);
  Repeat (50, [ Move (Translate {x = 1.0; y = 0.0}) ]);
  Repeat (100, [ Move (Translate {x = 0.0; y = -1.0}) ]);
  Repeat (100, [ Move (Translate {x = 1.0; y = 0.0}) ]);
  Move (Translate {x = 200.0; y = -200.0});
  Repeat (50, [ Move (Translate {x = 1.0; y = 0.0}) ]);
  Repeat (50, [ Move (Translate {x = 0.0; y = 1.0}) ]);
  Repeat (25, [ Move (Translate {x = 1.0; y = 0.0}) ]);
  Repeat (25, [ Move (Translate {x = 0.0; y = 1.0}) ]);
  Repeat (75, [ Move (Translate {x = -1.0; y = 0.0}) ]);
  Repeat (75, [ Move (Translate {x = 0.0; y = -1.0}) ]);
  Move (Translate {x = -400.0; y = 0.0});
  Repeat (75, [ Move (Translate {x = 0.0; y = 1.0}) ]);
  Repeat (75, [ Move (Translate {x = -1.0; y = 0.0}) ]);
  Repeat (25, [ Move (Translate {x = 0.0; y = -1.0}) ]);
  Repeat (25, [ Move (Translate {x = 1.0; y = 0.0}) ]);
  Repeat (50, [ Move (Translate {x = 0.0; y = -1.0}) ]);
  Repeat (50, [ Move (Translate {x = 1.0; y = 0.0}) ]);
]

let program4 = [
  Either (
    [ Repeat (100, [ Move (Translate {x = 1.0; y = 0.0}) ]) ], 
    [ Repeat (100, [ Move (Translate {x = 0.0; y = 1.0}) ]) ]
  );

  Repeat (100, [
    Either (
      [ Move (Translate {x = -1.0; y = 0.0}) ], 
      [ Move (Translate {x = 0.0; y = -1.0}) ]
    )
  ]);

  Either (
    [ Move (Translate {x = 10.0; y = 10.0}) ],
    [ Move (Translate {x = -10.0; y = -10.0}) ]
  )
]

(* Fonction principale *)
let () =
  let config = parse_args () in
  let size_spec = Printf.sprintf " %dx%d" config.width config.height in
  open_graph size_spec;

  (* Couleur de fond *)
  (match config.background_color with
  | Some c -> set_color c; fill_rect 0 0 config.width config.height
  | None -> ());

  (* Dessiner les axes*)
  draw_axes config.width config.height;

  (*Charger le programme selectionné *)
  let program =
    match config.prog_number with
    | 1 -> program1
    | 2 -> program2
    | 3 -> program3
    | 4 -> program4
    | _ -> failwith "Numero de programme invalide"
  in

  (* Si l'option -abs est spécifiée*)
  (match config.abs_rectangle with
   | Some rect ->
       let approximated_rect = over_approximate program rect in
       (match config.rectangle_color with
        | Some color -> draw_rectangle approximated_rect color 1.0
        | None -> draw_rectangle approximated_rect blue 1.0)
   | None -> ());

  (* Point initial *)
  let initial_point = { x = 0.0; y = 0.0 } in

  (* Execution du programme et affichage des positions *)
  let positions =
    try run program initial_point
    with Failure msg ->
      close_graph ();
      failwith ("Erreur lors de l'exécution du programme : " ^ msg)
  in

  let rec display_positions positions step =
    match positions with
    | [] -> ()
    | p :: rest ->
        (* Affichage du programme si demandé *)
        if config.print_code then begin
          Printf.printf "Step %d: %s\n%!" step (string_of_program program)
        end;

        (* Dessiner les points si l'option -cr est activée *)
        if config.display_points then
          let color = Option.value config.point_color ~default:red in
          let (x, y) = transform_point p config.width config.height 1.0 in
          set_color color;
          fill_circle x y 4;

        synchronize ();
        Unix.sleepf 0.001;
        display_positions rest (step + 1)
  in

  display_positions positions 0;

  (* Attendre un clic pour quitter *)
  let _ = wait_next_event [Button_down] in
  close_graph ();

