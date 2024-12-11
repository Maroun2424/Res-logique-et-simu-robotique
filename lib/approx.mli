val sample : Geo.rectangle -> Geo.point
val transform_rect : Geo.transformation -> Geo.rectangle -> Geo.rectangle
val run_rect : Interp.program -> Geo.rectangle -> Geo.rectangle list
val inclusion : Geo.rectangle -> Geo.rectangle -> bool
val target_reached_rect : Interp.program -> Geo.rectangle -> Geo.rectangle -> bool
val run_polymorphe : (Geo.transformation -> 'a -> 'a) -> Interp.program -> 'a -> 'a list
val over_approximate : Interp.program -> Geo.rectangle -> Geo.rectangle
val feasible_target_reached :
  Interp.program -> Geo.rectangle -> Geo.rectangle -> bool
