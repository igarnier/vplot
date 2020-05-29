open Vlayout

type style = Circle of float * Style.color | Square of float * Style.color

type initial_distribution = Sphere of float

type screen = { xres : int; yres : int; zoom : float }

type (_, _) data =
  | Graph :
      { impl : ('t, 'v, 'e) Graph_sig.impl; graph : 't; screen : screen }
      -> ('v, 'e) data

type 'v draw_vertex = 'v -> Pt.t -> Cmds.t list -> Cmds.t list

type 'e draw_edge = 'e -> Pt.t -> Pt.t -> Cmds.t list -> Cmds.t list

type ('v, 'e) options =
  [ `Initial_distribution of initial_distribution
  | `Initial_stiffness of float
  | `Global_position of Gg.P3.t
  | `Global_orientation of Gg.M3.t
  | `Relaxed_length of float
  | `Custom_vertices of 'v draw_vertex
  | `Custom_edges of 'e draw_edge ]

module Make (SM : Spring_model.S) : sig
  val draw_scene :
    obj_pos:Gg.v3 ->
    obj_rot:Gg.m3 ->
    camera:Camera.t ->
    model:'e SM.t ->
    draw_vertex:(SM.Table.key -> Vlayout.Pt.t -> Cmds.t list -> Cmds.t list) ->
    draw_edge:('e -> Vlayout.Pt.t -> Vlayout.Pt.t -> Cmds.t list -> Cmds.t list) ->
    Cmds.t list

  val plot :
    ('t, SM.vertex, 'e) Graph_sig.impl ->
    options:(SM.vertex, 'e) options list ->
    camera:Camera.t ->
    graph:'t ->
    Cmds.t

  val plot_anim :
    ('t, SM.vertex, 'e) Graph_sig.impl ->
    options:(SM.vertex, 'e) options list ->
    camera:Camera.t ->
    graph:'t ->
    unit ->
    Cmds.t
end
