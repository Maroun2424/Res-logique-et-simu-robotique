type coord2D = { x : float; y : float; }
type point = coord2D
type vector = coord2D
type angle = float
val translate : vector -> point -> point 
val rad_of_deg : angle -> angle 
val deg_of_rad : angle -> angle 
val rotate : point -> angle -> point -> point 
type transformation = Translate of vector | Rotate of point * angle
val transform : transformation -> point -> point 
type rectangle = {
  x_min : float;
  x_max : float;
  y_min : float;
  y_max : float;
}
val in_rectangle : rectangle -> point -> bool 
val corners : rectangle -> point list 
val rectangle_of_list : point list -> rectangle
