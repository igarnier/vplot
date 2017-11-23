(** We access trees labelled with type 'a abstractly by folding over them. *)
type 'a t =
  {
    fold : 'b. leaf:('a -> 'b) -> node:('a -> 'b list -> 'b) -> 'b
  }

val plot:
  lbl:('a -> Cmds.layout) ->
  data:'a t ->
  Cmds.layout
