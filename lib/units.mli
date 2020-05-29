type mm = private float

type cm = private float

type pt = private float

val mm : float -> mm

val cm : float -> cm

val pt : float -> pt

val mm_to_cm : mm -> cm

val cm_to_mm : cm -> mm

val mm_to_pt : mm -> pt

val pt_to_mm : pt -> mm

val pp_mm : Format.formatter -> mm -> unit

val pp_cm : Format.formatter -> cm -> unit

val pp_pt : Format.formatter -> pt -> unit
