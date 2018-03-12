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
  | `NoHeatbar
  ]

type options = [ hmap_options | Frame.options ]

type data =
  | Mat of { xdomain : Owl.Mat.mat; ydomain : Owl.Mat.mat; mat : Owl.Mat.mat }
  | Fun of { xdomain : Owl.Mat.mat; ydomain : Owl.Mat.mat; f : float -> float -> float }

val default_state : unit -> state

val default_gradient : gradient_spec
val white_gradient : gradient_spec

(* val plot_mat : *)
(*   options:options list -> *)
(*   viewport:Viewport.t -> *)
(*   xdomain:Owl.Mat.mat -> *)
(*   ydomain:Owl.Mat.mat -> *)
(*   data:Owl.Mat.mat -> Cmds.t *)

(* val plot_f : *)
(*   options:options list -> *)
(*   viewport:Viewport.t -> *)
(*   xdomain:Owl.Mat.mat -> *)
(*   ydomain:Owl.Mat.mat -> *)
(*   f:(float -> float -> float) -> Cmds.t *)

val plot :
  options:options list ->
  viewport:Viewport.t ->
  data:data -> Cmds.t
