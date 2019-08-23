open Gg

(* The camera is defined by its position, its rotation matrix and
   its "eye-to-screen" distance, which specifies indirectly the fov
   (a small eye-to-screen entails a wide camera angle). *)
type t =
  { pos : P3.t ;
    rot : M3.t;
    eyedist : float }

let init position eyedist angle axis =
  { pos = position ;
    rot = M3.rot3_axis axis angle ;
    eyedist = eyedist }

(* Converts a point from world space to camera space *)
let space_to_camera ~xres ~yres camera (p : P3.t) : V4.t =
  let cam_p = V3.sub p camera.pos in
  let rotmat = M3.transpose camera.rot in
  let cam_obj_pos = V3.ltr rotmat cam_p in
  (* project on screen *)
  let cx, cy, cz = V3.x cam_obj_pos, V3.y cam_obj_pos, V3.z cam_obj_pos in
  let scale = camera.eyedist /. (camera.eyedist -. cz) in
  let (x, y) = (scale *. cx +. (float (xres/2)),
                scale *. cy +. (float (yres/2))) in
  V4.v x y cz scale

(* Converts a point from camera space to world space, given a [z] coordinate *)
let camera_to_space ~xres ~yres camera (p : P2.t) (z : float) =
  let xres = float xres
  and yres = float yres in
  let x = (P2.x p) -. xres *. 0.5
  and y = (P2.y p) -. yres *. 0.5 in
  let iscale = (camera.eyedist -. z) /. camera.eyedist in
  let x = iscale *. x
  and y = iscale *. y in
  (* Camera space *)
  let p = V3.v x y camera.eyedist in
  (* World space *)
  let p = V3.ltr camera.rot p in
  (* Translate *)
  V3.add p camera.pos
