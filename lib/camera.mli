open Gg

type t

val init :
  position:P3.t ->
  eyedist:float ->
  zoom:float ->
  radians:float ->
  axis:V3.t ->
  t

val world_to_camera : t -> P3.t -> V3.t

val camera_to_world : xres:int -> yres:int -> t -> P2.t -> float -> V3.t

val project : t -> P3.t -> V4.t

val pos : t -> P3.t

val set_pos : t -> P3.t -> unit

val rot : t -> M3.t

val set_rot : t -> M3.t -> unit

val invrot : t -> M3.t

val eyedist : t -> float

val set_eyedist : t -> float -> unit

val zoom : t -> float

val set_zoom : t -> float -> unit
