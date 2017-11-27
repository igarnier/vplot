(** A [viewport] specifies the size (in pixels) of a plot. This specifies implicitly how to
    transform from the frame space of the plot to the screen space. *)
type t =
  (** [AutoX] fixes the height of the plot but let the width free so that the aspect ratio is preserved. *)
  | AutoX     of { ysize : int }
  (** [AutoY] fixes the width of the plot but let the height free so that the aspect ratio is preserved. *)
  | AutoY     of { xsize : int }
  (** [FixedSize] fixes the width /and/ the heighth of the plot, aspect ratio might not be preserved.  *)
  | FixedSize of { xsize : int; ysize : int }

val apply : t -> Cmds.t list -> Cmds.t