open Gg

type t = private
  { pos : P3.t ;
    rot : M3.t;
    eyedist : float }

val init : P3.t -> float -> float -> V3.t -> t

val space_to_camera : xres:int -> yres:int -> t -> P3.t -> V4.t
val camera_to_space : xres:int -> yres:int -> t -> P2.t -> float -> V3.t
