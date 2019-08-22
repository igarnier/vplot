
type vec =
  | Full of { data: Numerics.Float64.Vec.t; sty: Vlayout.Style.t; lab: string }
  | Simple of Numerics.Float64.Vec.t

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

type data = { domain : Numerics.Float64.Vec.t; vecs : vec list }

val plot :
  options:options list ->
  viewport:Viewport.t ->
  data:data -> Cmds.t
