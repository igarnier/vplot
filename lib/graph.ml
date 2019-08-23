open Gg
open Vlayout

type color = Style.color

type style =
  | Circle of float * color  (* radius * color *)
  | Square of float * color  (* width * color *)

type initial_distribution =
  | Sphere of float          (* radius *)

type (_, _) data = Graph : {
    impl : ('t, 'v, 'e) Graph_sig.impl ;
    graph : 't ;
  } -> ('v, 'e) data

type 'v draw_vertex = 'v -> Pt.t -> Cmds.t list -> Cmds.t list
type 'e draw_edge = 'e -> Pt.t -> Pt.t -> Cmds.t list -> Cmds.t list

type ('v, 'e) options =
  [ `Initial_distribution of initial_distribution
  | `Initial_stiffness of float
  | `Relaxed_length of float
  | `Custom_vertices of 'v draw_vertex
  | `Custom_edges of 'e draw_edge ]

type ('v, 'e) graph_options =
  { initial_distribution : initial_distribution ;
    initial_stiffness : float ;
    relaxed_length : float ;
    draw_vertex : 'v draw_vertex ;
    draw_edge : 'e draw_edge }

let default_draw_vertex =
  fun _ p acc -> (Cmds.circle ~center:p ~radius:10.0) :: acc

let default_draw_edge =
  fun _ p1 p2 acc -> (Cmds.segment ~p1 ~p2) :: acc

let default_options =
  { initial_distribution = Sphere 100.0 ;
    initial_stiffness = 1.0 ;
    relaxed_length = 100.0 ;
    draw_vertex = default_draw_vertex ;
    draw_edge = default_draw_edge }

let set_option (graphopts : ('v, 'e) graph_options) (opt : ('v, 'e) options) =
  match opt with
  | `Initial_distribution initial_distribution ->
    { graphopts with initial_distribution }
  | `Relaxed_length relaxed_length ->
    { graphopts with relaxed_length }
  | `Custom_edges draw_edge ->
    { graphopts with draw_edge }
  | `Initial_stiffness initial_stiffness ->
    { graphopts with initial_stiffness }
  | `Custom_vertices draw_vertex ->
    { graphopts with draw_vertex }

let set_options opts =
  List.fold_left set_option default_options opts

let graph_to_spring_model :
  type t v e.
  (t, v, e) Graph_sig.impl ->
  t ->
  dist:initial_distribution ->
  stiff:float ->
  relax:float ->
  (t, v, e) Spring_model.boxed =
  fun (module Graph) graph ~dist ~stiff ~relax ->
  let radius =
    match dist with | Sphere radius -> radius in
  let module Result =
  struct
    module G = Graph
    module Vertex_map = Map.Make(Graph.V)
    let graph = graph
    let nodes = Graph.fold_vertex (fun v map ->
        let neighbours =
          Graph.fold_succ (fun v' edges ->
              if G.V.compare v v' = -1 then
                let e = G.find_edge graph v v' in
                (v', e, stiff) :: edges
              else
                edges
            ) graph v [] in
        let position = Spring_model.spherical_configuration radius in
        Vertex_map.add v { Spring_model.position ;
                           velocity = V3.zero ;
                           force    = V3.zero ;
                           neighbours } map
      ) graph Vertex_map.empty
    let relax_length = relax
  end in
  (module Result)

(* ------------------------------------------------------------------------- *)
(* Rendering functions *)

(* moves an array of point such that it is at the left center of the screen, at a distance z *)
(* let to_left_center xres yres camera points = *)
(*   let (mins, maxs) = Float3.bbox points in *)
(*   let extents      = Float3.sub maxs mins in *)
(*   let target       = camera_to_space xres yres camera (-. (float yres), 0.0) extents.(2) in *)
(*   let delta        = Float3.sub  *)

(* Render process: for each object, compute its (x,y,z) coordinates in camera space,
   i.e. by 1) substracting camera origin
   and 2) multiplying by the transpose (= the inverse, by orthonormality) of the
   camera's rotation matrix.
   Once in camera space, project on screen space. Draw primitives scaled in proportion
   of their distance from the camera center. *)
let draw_scene :
  type t v e.
  xres:int ->
  yres:int ->
  camera:Camera.t ->
  spring:(t, v, e) Spring_model.boxed ->
  draw_vertex:(v -> Pt.t -> Cmds.t list -> Cmds.t list) ->
  draw_edge:(e -> Pt.t -> Pt.t -> Cmds.t list -> Cmds.t list) ->
  Cmds.t list =
  fun ~xres ~yres ~camera ~spring ~draw_vertex ~draw_edge ->
  let module Spring = (val spring) in
  let open Spring in
  let transformed =
    Vertex_map.map (fun { Spring_model.position ; _ } ->
        Camera.space_to_camera ~xres ~yres camera position
      ) nodes in
  let prj v4 = Pt.pt (V4.x v4) (V4.y v4) in
  let edges =
    Vertex_map.fold (fun v1 { Spring_model.neighbours ; _ } acc ->
        List.fold_left (fun acc (v2, e, _stiffness) ->
            let p1 = prj @@ Vertex_map.find v1 transformed in
            let p2 = prj @@ Vertex_map.find v2 transformed in
            draw_edge e p1 p2 acc
          ) acc neighbours
      ) nodes [] in
  let nodes =
    Vertex_map.bindings transformed
    |> Array.of_list in
  (* Now draw objects. Use a false pointer array to index nodes. *)
  let nodes_index = Array.init (Array.length nodes) (fun i -> i) in
  Array.sort (fun i1 i2 ->
      let z1 = V4.z (snd nodes.(i1)) in
      let z2 = V4.z (snd nodes.(i2)) in
      if z1 < z2 then 1 else -1
    ) nodes_index ;
  Array.fold_left (fun acc i ->
      let vtx, vtx_pos = nodes.(nodes_index.(i)) in
      draw_vertex vtx (prj vtx_pos) acc
    ) edges nodes_index

let plot :
  type t v e.
  (t, v, e) Graph_sig.impl ->
  options:(v, e) options list ->
  viewport:Viewport.t ->
  camera:Camera.t ->
  graph:t ->
  Cmds.t =
  fun impl ~options ~viewport ~camera ~graph ->
  let options = set_options options in
  let spring =
    graph_to_spring_model
      impl
      graph
      ~dist:options.initial_distribution
      ~stiff:options.initial_stiffness
      ~relax:options.relaxed_length in
  Spring_model.relax spring ;
  let result =
    draw_scene ~xres:800 ~yres:800 ~camera ~spring
      ~draw_vertex:options.draw_vertex
      ~draw_edge:options.draw_edge in
  Viewport.apply viewport result

let plot_anim :
  type t v e.
  (t, v, e) Graph_sig.impl ->
  options:(v, e) options list ->
  viewport:Viewport.t ->
  camera:Camera.t ->
  graph:t ->
  unit -> Cmds.t =
  fun impl ~options ~viewport ~camera ~graph ->
  let options = set_options options in
  let spring =
    graph_to_spring_model
      impl
      graph
      ~dist:options.initial_distribution
      ~stiff:options.initial_stiffness
      ~relax:options.relaxed_length in
  let integrator = Spring_model.make_integrator spring 1000 in
  fun () ->
    integrator () ;
    let result =
      draw_scene ~xres:800 ~yres:800 ~camera ~spring
        ~draw_vertex:options.draw_vertex
        ~draw_edge:options.draw_edge in
    Viewport.apply viewport result
