(** A [viewport] specifies the size (in pixels) of a plot. This specifies
    implicitly how to transform from the frame space of the plot to the
    screen space. *)
type t =
  | AutoX of { ysize : Units.mm }
      (** [AutoX] fixes the height of the plot but let the width free so that
      the aspect ratio is preserved. *)
  | AutoY of { xsize : Units.mm }
      (** [AutoY] fixes the width of the plot but let the height free so that
      the aspect ratio is preserved. *)
  | FixedSize of { xsize : Units.mm; ysize : Units.mm }
      (** [FixedSize] fixes the width /and/ the heighth of the plot, aspect ratio
      might not be preserved.  *)

val apply : t -> Cmds.t -> Cmds.t
