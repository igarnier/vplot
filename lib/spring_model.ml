(* Implemenation of a spring model relaxation algorithm. We have objects in
   3d, with springs attached on some of them (no self-spring). Springs are
   parameterised by a stiffness constant. We want to minimise the total
   potential energy by adjusting the positions of the objects. We approximate
   this by computing the classical dynamics of each object, taking into account
   a global drag force. We stop whenever the total energy is below a given
   threshold.
   Our implementation is heavily imperative. *)

open Gg

type ('v, 'e) node_state =
  { mutable position : P3.t ;
    mutable velocity : V3.t ;
    mutable force : V3.t ;
    neighbours : ('v * 'e * float) list }

module type S =
sig
  module G : Graph_sig.S

  module Vertex_map : Map.S with type key = G.V.t

  val graph : G.t
  val nodes : (G.vertex, G.edge) node_state Vertex_map.t
  val relax_length : float
end

type ('t, 'v, 'e) boxed = (module S with type G.vertex = 'v
                                     and type G.edge = 'e
                                     and type G.t = 't)

(* Compute the force applied on each given object by all the other ones, taking into
   account a drag parameter (typically, drag ∈ [0;1]) *)
let compute_forces : type t v e. (t, v, e) boxed -> drag_factor:float -> unit =
  fun (module Spring_model) ~drag_factor ->
  let open Spring_model in
  Vertex_map.iter (fun _v state ->
      state.force <- V3.zero
    ) nodes ;
  Vertex_map.iter (fun _v state ->
      List.iter (fun (n, _e, stiffness) ->
          let state_n = Vertex_map.find n nodes in
          let direction = V3.sub state_n.position state.position in (* from i to target *)
          let ndir = V3.norm direction in
          let delta_rel = ndir -. relax_length in
          let force = delta_rel *. stiffness in
          let forcevec = V3.smul (force /. ndir) direction in
          let drag = V3.smul (~-. drag_factor) state.velocity in
          state.force <- V3.(state.force + drag + forcevec) ;
          state_n.force <- V3.(state_n.force + drag + (V3.neg forcevec))
        ) state.neighbours
    ) nodes

(* We use a classical (and imprecise) forward euler integration step. We also assume that
   each object has unit mass. *)
let integrate : type v e t. (v, e, t) boxed -> delta_t:float -> unit =
  fun (module Spring_model) ~delta_t ->
  let open Spring_model in
  Vertex_map.iter (fun _v state ->
      state.position <- V3.(state.position + (smul delta_t state.velocity)) ;
      state.velocity <- V3.(state.velocity + (smul delta_t state.force))
    ) nodes

(* Given [n] objects, we distribute them uniformy on the surface of a sphere
   of radius [radius]. Note that in order to make sense, this radius sould be so that
   the average distance between two points corresponds to the relaxation length. *)
let pi = acos (~-. 1.0)

let spherical_configuration radius =
  let theta0 = Random.float (2.0 *. pi) in
  let theta1 = acos (1.0 -. (Random.float 2.0)) in
  let x = radius *. (sin theta0) *. (sin theta1) in
  let y = radius *. (cos theta0) *. (sin theta1) in
  let z = radius *. (cos theta1) in
  V3.v x y z

let perform_relaxation_step graph ~drag_factor ~delta_t =
  compute_forces graph ~drag_factor ;
  integrate graph ~delta_t

let total_enery : type t v e. (t, v, e) boxed -> float =
  fun (module Spring_model) ->
  let open Spring_model in
  (* kinetic energy = 1/2 * m * v^2. We assume m = 1. *)
  let res =
    Vertex_map.fold (fun _v state acc ->
        (V3.norm2 state.velocity) +. acc
      ) nodes 0.0 in
  0.5 *. res

let make_integrator : type t v e. (t, v, e) boxed -> int -> unit -> unit =
  fun spring_model steps ->
  let steps = ref steps in
  perform_relaxation_step spring_model ~drag_factor:1.0 ~delta_t:0.1 ;
  let start_energy = total_enery spring_model in
  let energy = ref start_energy in
  fun () ->
    if !steps > 0 && !energy > 0.1 *. start_energy then
      (perform_relaxation_step spring_model ~drag_factor:1.0 ~delta_t:0.1 ;
       decr steps ;
       energy := total_enery spring_model)
    else
      ()

let relax : type t v e. (t, v, e) boxed -> unit =
  fun spring_model ->
  let steps = ref 1000 in
  perform_relaxation_step spring_model ~drag_factor:1.0 ~delta_t:0.1 ;
  let start_energy = total_enery spring_model in
  let energy = ref start_energy in
  while !steps > 0 && !energy > 0.1 *. start_energy do
    perform_relaxation_step spring_model ~drag_factor:1.0 ~delta_t:0.1 ;
    decr steps ;
    energy := total_enery spring_model
  done
