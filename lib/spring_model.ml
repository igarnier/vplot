open Gg

module type Vertex_sig = sig
  type t

  val hash : t -> int

  val equal : t -> t -> bool

  val compare : t -> t -> int
end

module type S = sig
  type vertex

  module Table : Hashtbl.S with type key = vertex

  type 'e t =
    { (* total force applied on each object *)
      state : 'e node_state Table.t;
      (* edges/springs *)
      relax_length : float;
      (* relaxed length *)
      stiffness : float
    }

  and 'e node_state =
    { mutable prev_position : P3.t;
      mutable position : P3.t;
      mutable velocity : V3.t;
      mutable force : V3.t;
      mutable neighbours : (vertex * 'e * float) list;
      mutable is_anchor : bool
    }

  val create : relax_length:float -> stiffness:float -> 'e t

  val recenter : P3.t -> 'e t -> unit

  val mem_edge : 'e t -> vertex -> vertex -> bool

  val add_vertex : 'e t -> vertex -> V3.t option -> unit

  val remove_vertex : 'e t -> vertex -> unit

  val add_edge : 'e t -> vertex -> 'e -> vertex -> unit

  val remove_edge : 'e t -> vertex -> vertex -> unit

  val make_integrator : steps:int option -> model:'e t -> unit -> unit

  val perform_relaxation_step :
    'a t -> drag_factor:float -> delta_t:float -> unit

  val relax : steps:int option -> model:'e t -> unit
end

module Make (V : Vertex_sig) : S with type vertex = V.t = struct
  type vertex = V.t

  module Table = Hashtbl.Make (struct
    type t = vertex

    let hash = V.hash

    let equal = V.equal
  end)

  type 'e t =
    { (* total force applied on each object *)
      state : 'e node_state Table.t;
      (* edges/springs *)
      relax_length : float;
      (* relaxed length *)
      stiffness : float
    }

  and 'e node_state =
    { mutable prev_position : P3.t;
      mutable position : P3.t;
      mutable velocity : V3.t;
      mutable force : V3.t;
      mutable neighbours : (vertex * 'e * float) list;
      mutable is_anchor : bool
    }

  let pi = acos ~-.1.

  let recenter pos { state; _ } =
    let bbox =
      Table.fold
        (fun _ { position; _ } bbox -> Box3.add_pt bbox position)
        state
        Box3.empty
    in
    let mid = Box3.mid bbox in
    let delta = V3.sub pos mid in
    Table.iter
      (fun _ state -> state.position <- V3.add state.position delta)
      state

  let spherical_configuration radius =
    let theta0 = Random.float (2.0 *. pi) in
    let theta1 = acos (1.0 -. Random.float 2.0) in
    let x = radius *. sin theta0 *. sin theta1 in
    let y = radius *. cos theta0 *. sin theta1 in
    let z = radius *. cos theta1 in
    V3.v x y z

  let compute_forces (model : 'e t) (drag : float) =
    (* reset force acc *)
    Table.iter (fun _ state -> state.force <- V3.zero) model.state ;
    Table.iter
      (fun _v0 state ->
        let pos0 = state.position in
        let drag = V3.smul ~-.drag state.velocity in
        state.force <- V3.add drag state.force ;
        List.iter
          (fun (v1, _e, stiffness) ->
            let state1 = Table.find model.state v1 in
            let pos1 = state1.position in
            let dir1 = V3.sub pos1 pos0 in
            let u = V3.unit dir1 in
            state.force <- V3.add state.force (V3.smul stiffness dir1) ;
            List.iter
              (fun (v2, _e, _stiff) ->
                if V.equal v1 v2 then ()
                else
                  let state2 = Table.find model.state v2 in
                  let pos2 = state2.position in
                  let v = V3.unit (V3.sub pos2 pos0) in
                  let n = V3.cross u v in
                  let r = pi -. asin (V3.norm n) in
                  let f1 = V3.smul r @@ V3.cross u n in
                  let f2 = V3.smul r @@ V3.cross n v in
                  state1.force <- V3.add state1.force f1 ;
                  state2.force <- V3.add state2.force f2)
              state.neighbours)
          state.neighbours)
      model.state

  let verlet model delta_t =
    Table.iter
      (fun _v state ->
        if state.is_anchor then ()
        else
          let prev_position = state.position in
          let open V3 in
          state.position <-
            V3.smul 2. state.position - state.prev_position
            + V3.smul (delta_t *. delta_t) state.force ;
          state.velocity <-
            V3.smul (1. /. delta_t) (state.position - state.prev_position) ;
          state.prev_position <- prev_position)
      model.state

  let integrate = verlet

  (* Given [n] objects, we distribute them uniformy on the surface of a sphere
     of radius [radius]. Note that in order to make sense, this radius sould be so that
     the average distance between two points corresponds to the relaxation length. *)

  let create ~relax_length ~stiffness =
    { state = Table.create 503;
      (* arbitrary initial size (prime, because ...) *)
      relax_length;
      stiffness
    }

  let canonical_order v1 v2 =
    let c = V.compare v1 v2 in
    assert (c <> 0) ;
    if c = -1 then (v1, v2) else (v2, v1)

  let add_vertex (model : 'e t) (v : vertex) (position : V3.t option) =
    let position =
      match position with
      | None ->
          (* kind of arbitrary position *)
          spherical_configuration model.relax_length
      | Some p -> p
    in
    let state =
      { prev_position = position;
        position;
        velocity = V3.zero;
        force = V3.zero;
        neighbours = [];
        is_anchor = false
      }
    in
    Table.add model.state v state

  let remove_vertex (model : 'e t) (v : vertex) =
    assert (Table.mem model.state v) ;
    let state = Table.find model.state v in
    List.iter
      (fun (v', _, _) ->
        let state' = Table.find model.state v' in
        state'.neighbours <-
          List.filter (fun (v', _, _) -> not (V.equal v' v)) state'.neighbours)
      state.neighbours ;
    Table.remove model.state v

  let mem_edge (model : 'e t) (v1 : vertex) (v2 : vertex) =
    assert (Table.mem model.state v1) ;
    assert (Table.mem model.state v2) ;
    let (v1, v2) = canonical_order v1 v2 in
    let state = Table.find model.state v1 in
    List.exists (fun (v2', _, _) -> V.equal v2 v2') state.neighbours

  let add_edge (model : 'e t) (v1 : vertex) (e : 'e) (v2 : vertex) =
    assert (Table.mem model.state v1) ;
    assert (Table.mem model.state v2) ;
    let (v1, v2) = canonical_order v1 v2 in
    let state = Table.find model.state v1 in
    state.neighbours <- (v2, e, model.stiffness) :: state.neighbours ;
    let state = Table.find model.state v2 in
    state.neighbours <- (v1, e, model.stiffness) :: state.neighbours

  let remove_edge (model : 'e t) (v1 : vertex) (v2 : vertex) =
    assert (Table.mem model.state v1) ;
    assert (Table.mem model.state v2) ;
    let v1_state = Table.find model.state v1 in
    v1_state.neighbours <-
      List.filter (fun (v', _, _) -> not (V.equal v' v2)) v1_state.neighbours ;
    let v2_state = Table.find model.state v2 in
    v2_state.neighbours <-
      List.filter (fun (v', _, _) -> not (V.equal v' v1)) v2_state.neighbours

  let perform_relaxation_step graph ~drag_factor ~delta_t =
    compute_forces graph drag_factor ;
    integrate graph delta_t

  let total_energy model =
    (* kinetic energy = 1/2 * m * v^2. We assume m = 1. *)
    let res =
      Table.fold
        (fun _v state acc -> V3.norm2 state.velocity +. acc)
        model.state
        0.0
    in
    0.5 *. res

  let make_integrator ~steps ~model =
    let steps = ref (Option.value ~default:1000 steps) in
    perform_relaxation_step model ~drag_factor:1.0 ~delta_t:0.1 ;
    let start_energy = total_energy model in
    let energy = ref start_energy in
    fun () ->
      if !steps > 0 && !energy > 0.001 *. start_energy then (
        perform_relaxation_step model ~drag_factor:1.0 ~delta_t:0.1 ;
        decr steps ;
        energy := total_energy model )
      else ()

  let relax ~steps ~model =
    let steps = ref (Option.value ~default:1000 steps) in
    perform_relaxation_step model ~drag_factor:1.0 ~delta_t:0.1 ;
    let start_energy = total_energy model in
    let energy = ref start_energy in
    while !steps > 0 && !energy > 0.001 *. start_energy do
      decr steps ;
      energy := total_energy model ;
      perform_relaxation_step model ~drag_factor:1.0 ~delta_t:0.1
    done
end
