type vec =
  | Full of { data: Owl.Mat.mat; sty: Vlayout.Style.t; lab: string }
  | Simple of Owl.Mat.mat

type state = {
  mutable min_value : float;
  mutable max_value : float
}

type vector_options =
  [ `State of state ]

type options =
  [ vector_options
  | Frame.options
  ]

type data = { domain : Owl.Mat.mat; vecs : vec list }

val plot :
  options:options list ->
  viewport:Viewport.t ->
  data:data -> Cmds.t
