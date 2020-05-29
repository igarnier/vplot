open Gg
open Vlayout

type color = Style.color

type style = Circle of float * color | Square of float * color

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
  | `Global_position of P3.t
  | `Global_orientation of M3.t
  | `Relaxed_length of float
  | `Custom_vertices of 'v draw_vertex
  | `Custom_edges of 'e draw_edge ]

type ('v, 'e) graph_options =
  { initial_distribution : initial_distribution;
    initial_stiffness : float;
    global_position : P3.t;
    global_orientation : M3.t;
    relaxed_length : float;
    draw_vertex : 'v draw_vertex;
    draw_edge : 'e draw_edge
  }

let default_draw_vertex _ p acc = Cmds.circle ~center:p ~radius:10.0 :: acc

let default_draw_edge _ p1 p2 acc = Cmds.segment ~p1 ~p2 :: acc

let default_options =
  { initial_distribution = Sphere 100.0;
    initial_stiffness = 1.0;
    global_position = P3.o;
    global_orientation = M3.id;
    relaxed_length = 100.0;
    draw_vertex = default_draw_vertex;
    draw_edge = default_draw_edge
  }

let set_option (graphopts : ('v, 'e) graph_options) (opt : ('v, 'e) options) =
  match opt with
  | `Initial_distribution initial_distribution ->
      { graphopts with initial_distribution }
  | `Relaxed_length relaxed_length -> { graphopts with relaxed_length }
  | `Global_position global_position -> { graphopts with global_position }
  | `Global_orientation global_orientation ->
      { graphopts with global_orientation }
  | `Custom_edges draw_edge -> { graphopts with draw_edge }
  | `Initial_stiffness initial_stiffness -> { graphopts with initial_stiffness }
  | `Custom_vertices draw_vertex -> { graphopts with draw_vertex }

let set_options opts = List.fold_left set_option default_options opts

(* ------------------------------------------------------------------------- *)
(* Rendering functions *)

module Make (SM : Spring_model.S) = struct
  let object_to_world v obj_rot obj_pos =
    let wld_vertex = V3.ltr obj_rot v in
    V3.add wld_vertex obj_pos

  let draw_scene ~obj_pos ~obj_rot ~camera ~(model : 'e SM.t) ~draw_vertex
      ~draw_edge : Cmds.t list =
    (* transform and project the node on the screen *)
    let projected = SM.Table.create (SM.Table.length model.state) in
    SM.Table.iter
      (fun v { SM.position; neighbours; _ } ->
        (* transform point to world space *)
        let wld_vertex = object_to_world position obj_rot obj_pos in
        (* transform point to camera space *)
        let cam_vertex = Camera.world_to_camera camera wld_vertex in
        (* project to screen *)
        let scr_vertex = Camera.project camera cam_vertex in
        (* project on screen *)
        let result = (scr_vertex, neighbours) in
        SM.Table.add projected v result)
      model.state ;
    let prj v = Pt.pt (V4.x v) (V4.y v) in
    (* Now draw objects. Use a false pointer array to index nodes. *)
    let nodes = Array.of_seq (SM.Table.to_seq projected) in
    let nodes_index = Array.init (Array.length nodes) (fun i -> i) in
    Array.sort
      (fun i1 i2 ->
        let (_, (v1, _)) = nodes.(i1) in
        let (_, (v2, _)) = nodes.(i2) in
        let z1 = V4.z v1 in
        let z2 = V4.z v2 in
        if z1 < z2 then 1 else -1)
      nodes_index ;
    let nodes =
      Array.fold_left
        (fun acc i ->
          let (vtx, (pos, _)) = nodes.(nodes_index.(i)) in
          draw_vertex vtx (prj pos) acc)
        []
        nodes_index
    in
    SM.Table.fold
      (fun _v1 (p1, neighbours) acc ->
        List.fold_left
          (fun acc (v2, e, _stiffness) ->
            let (p2, _) = SM.Table.find projected v2 in
            draw_edge e (prj p1) (prj p2) acc)
          acc
          neighbours)
      projected
      nodes

  let graph_to_spring_model :
      type t e.
      (t, SM.vertex, e) Graph_sig.impl ->
      t ->
      dist:initial_distribution ->
      stiffness:float ->
      relax_length:float ->
      e SM.t =
   fun (module Graph) graph ~dist ~stiffness ~relax_length ->
    ignore dist ;
    let sm = SM.create ~relax_length ~stiffness in
    Graph.fold_vertex (fun v () -> SM.add_vertex sm v None) graph () ;
    Graph.fold_edges
      (fun v v' () ->
        let e = Graph.find_edge graph v v' in
        SM.add_edge sm v e v')
      graph
      () ;
    sm

  let plot :
      type t e.
      (t, SM.vertex, e) Graph_sig.impl ->
      options:(SM.vertex, e) options list ->
      camera:Camera.t ->
      graph:t ->
      Cmds.t =
   fun impl ~options ~camera ~graph ->
    let options = set_options options in
    let model =
      graph_to_spring_model
        impl
        graph
        ~dist:options.initial_distribution
        ~stiffness:options.initial_stiffness
        ~relax_length:options.relaxed_length
    in
    SM.relax ~steps:None ~model ;
    let result =
      draw_scene
        ~obj_pos:options.global_position
        ~obj_rot:options.global_orientation
        ~camera
        ~model
        ~draw_vertex:options.draw_vertex
        ~draw_edge:options.draw_edge
    in
    Cmds.wrap ~subcommands:result

  let plot_anim :
      type t e.
      (t, SM.vertex, e) Graph_sig.impl ->
      options:(SM.vertex, e) options list ->
      camera:Camera.t ->
      graph:t ->
      unit ->
      Cmds.t =
   fun impl ~options ~camera ~graph ->
    let options = set_options options in
    let model =
      graph_to_spring_model
        impl
        graph
        ~dist:options.initial_distribution
        ~stiffness:options.initial_stiffness
        ~relax_length:options.relaxed_length
    in
    let integrator = SM.make_integrator ~steps:(Some 1000) ~model in
    fun () ->
      integrator () ;
      let result =
        draw_scene
          ~obj_pos:options.global_position
          ~obj_rot:options.global_orientation
          ~camera
          ~model
          ~draw_vertex:options.draw_vertex
          ~draw_edge:options.draw_edge
      in
      Cmds.wrap ~subcommands:result
end
