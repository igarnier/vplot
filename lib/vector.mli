type vec =
  | Full of { data: Dense_float64_vec.t; sty: Vlayout.Style.t; lab: string }
  | Simple of Dense_float64_vec.t

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

type data = { domain : Dense_float64_vec.t; vecs : vec list }

val plot :
  options:options list ->
  viewport:Viewport.t ->
  data:data -> Cmds.t
