type vec =
  | Full of { data: Owl.Vec.vec; sty: Vlayout.Style.t; lab: string }
  | Simple of Owl.Vec.vec

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

type data = { domain : Owl.Vec.vec; vecs : vec list }

val plot :
  options:options list ->
  viewport:Viewport.t ->
  data:data -> Cmds.t
