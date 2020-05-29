open Gg

type t = private
  { pos : P3.t; rot : M3.t; invrot : M3.t; eyedist : float; zoom : float }

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

val rot : t -> M3.t

val eyedist : t -> float
