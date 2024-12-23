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
  prog_number = 1;
  print_code = false;                        (* Par défaut, pas d'affichage terminal *)
}
let () =
  try loop 5
  with Quit -> close_graph ();;
