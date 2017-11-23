type gradient_spec =
  { gradient_path : Gradient.path; gradient_steps : int }

type state = {
  mutable min_value  : float;
  mutable max_value  : float;
}

type hmap_options =
  [ `Gradient of gradient_spec
  | `Blocksize of int * int
  | `State of state
  | `Axis of Frame.axis_option list
  ]

type options = [ hmap_options | Frame.options ]

type data =
  | Mat of { xdomain : Owl.Vec.vec; ydomain : Owl.Vec.vec; mat : Owl.Mat.mat }
  | Fun of { xdomain : Owl.Vec.vec; ydomain : Owl.Vec.vec; f : float -> float -> float }

val default_state : unit -> state

(* val plot_mat : *)
(*   options:options list -> *)
(*   viewport:Viewport.t -> *)
(*   xdomain:Owl.Vec.vec -> *)
(*   ydomain:Owl.Vec.vec -> *)
(*   data:Owl.Mat.mat -> Cmds.t *)

(* val plot_f : *)
(*   options:options list -> *)
(*   viewport:Viewport.t -> *)
(*   xdomain:Owl.Vec.vec -> *)
(*   ydomain:Owl.Vec.vec -> *)
(*   f:(float -> float -> float) -> Cmds.t *)

val plot :
  options:options list ->
  viewport:Viewport.t ->
  data:data -> Cmds.t
