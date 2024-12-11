type instruction =
    Move of Geo.transformation
  | Repeat of int * program
  | Either of program * program
and program = instruction list
val is_deterministic : program -> bool
val unfold_repeat : program -> program
val run_det : program -> Geo.point -> Geo.point list
val target_reached_det : program -> Geo.point -> Geo.rectangle -> bool
val run : program -> Geo.point -> Geo.point list
val all_choices : program -> instruction list list
val target_reached : program -> Geo.point -> Geo.rectangle -> bool
