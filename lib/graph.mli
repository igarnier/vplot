open Vlayout

type style =
  | Circle of float * Style.color
  | Square of float * Style.color

type initial_distribution = Sphere of float

type (_, _) data = Graph : {
    impl : ('t, 'v, 'e) Graph_sig.impl ;
    graph : 't
  } -> ('v, 'e) data


type 'v draw_vertex = 'v -> Pt.t -> Cmds.t list -> Cmds.t list
type 'e draw_edge = 'e -> Pt.t -> Pt.t -> Cmds.t list -> Cmds.t list

type ('v, 'e) options =
  [ `Initial_distribution of initial_distribution
  | `Initial_stiffness of float
  | `Relaxed_length of float
  | `Custom_vertices of 'v draw_vertex
  | `Custom_edges of 'e draw_edge ]

val plot :
  ('t, 'v, 'e) Graph_sig.impl ->
  options:('v, 'e) options list ->
  viewport:Viewport.t ->
  camera:Camera.t ->
  graph:'t ->
  Cmds.t

val plot_anim :
  ('t, 'v, 'e) Graph_sig.impl ->
  options:('v, 'e) options list ->
  viewport:Viewport.t ->
  camera:Camera.t ->
  graph:'t ->
  (unit -> Cmds.t)
