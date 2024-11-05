
(* Code de la Section 3 du projet. *)

type coord2D = {
    x : float;
    y : float
  }
type point = coord2D
type vector = coord2D
type angle = float

(* Image du point p par la translation de vecteur v *)
let translate (v : vector) (p : point) : point =
  { x = v.x +. p.x; y = v.y +. p.y }

(* Convertir un angle de degrés en radians *)
let rad_of_deg (a : angle) : angle =
  a *. Float.pi /. 180.0

(* Convertir un angle de radians en degrés *)
let deg_of_rad (a : angle) : angle =
  a *. 180.0 /. Float.pi

(* Image du point p par la rotation d'angle alpha et de centre c *)
let rotate (c : point) (alpha : angle) (p : point) : point =
  let theta = rad_of_deg alpha in
  { x = c.x +. (p.x -. c.x )*. Float.cos theta -. (p.y -. c.y)*. Float.sin theta ;
    y = c.y +. (p.x -. c.x )*. Float.sin theta +. (p.y -. c.y)*. Float.cos theta }
  
type transformation =
  Translate of vector
| Rotate of point * angle

let transform (t : transformation) (p : point) : point =
  match t with
  | Translate v -> translate v p
  | Rotate (c, alpha) -> rotate c alpha p

type rectangle = {
    x_min : float;
    x_max : float;
    y_min : float;
    y_max : float
  }

let in_rectangle (r : rectangle) (p : point) : bool =
  failwith "À compléter"

let corners (r :rectangle) : point list =
  failwith "À compléter"
  
let rectangle_of_list (pl : point list) : rectangle = 
  failwith "À compléter"
