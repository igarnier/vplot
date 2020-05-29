open Gg

(* The camera is defined by its position, its rotation matrix and
   its "eye-to-screen" distance, which specifies indirectly the fov
   (a small eye-to-screen entails a wide camera angle). *)
type t =
  { pos : P3.t; rot : M3.t; invrot : M3.t; eyedist : float; zoom : float }

let init ~position ~eyedist ~zoom ~radians ~axis =
  assert (eyedist > 0.0) ;
  let rot = M3.rot3_axis axis radians in
  { pos = position; rot; invrot = M3.inv rot; eyedist; zoom }

(* Converts a point from world space to camera space *)
let world_to_camera camera (p : P3.t) : V3.t =
  let p = V3.sub p camera.pos in
  V3.ltr camera.invrot p

(* Converts a point from camera space to world space, given a [z] coordinate *)
let camera_to_world ~xres ~yres camera (p : P2.t) (z : float) =
  let xres = float xres and yres = float yres in
  let x = P2.x p -. (xres *. 0.5) and y = P2.y p -. (yres *. 0.5) in
  let iscale = (camera.eyedist -. z) /. camera.eyedist in
  let x = iscale *. x and y = iscale *. y in
  (* Camera space *)
  let p = V3.v x y camera.eyedist in
  (* World space *)
  let p = V3.ltr camera.rot p in
  (* Translate *)
  V3.add p camera.pos

let pos { pos; _ } = pos

let rot { rot; _ } = rot

let eyedist { eyedist; _ } = eyedist

(* Project a point to the screen *)
let project camera (p : P3.t) : V4.t =
  let (cx, cy, cz) = V3.to_tuple p in
  let scale = camera.eyedist /. (camera.eyedist -. cz) in
  let x = camera.zoom *. scale *. cx in
  let y = camera.zoom *. scale *. cy in
  V4.v x y cz scale
