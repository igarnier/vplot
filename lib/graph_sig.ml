
(* A subset of OcamlGraph's signature. *)
module type S =
sig
  type t
  type vertex
  type edge

  module V : sig
    type t = vertex
    val compare : t -> t -> int
  end

  val nb_vertex : t -> int
  val nb_edges : t -> int
  val find_edge : t -> vertex -> vertex -> edge

  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
end

type ('t, 'v, 'e) impl =
  (module S with type t = 't
             and type vertex = 'v
             and type edge = 'e)
