(** The kind of plots that can be performed. *)
type plot =
  | Heatmap of { data : Heatmap.data; options : Heatmap.options list }
  | Vector  of { data : Vector.data;  options : Vector.options list }
  | Scatter of { data : Scatter.data; options : Scatter.options list }

(** A layout is a bunch of plots organized hierarchically in [plotbox]es. *)
type layout =
  | Plot of { vp : Viewport.t; plot : plot }
  | Cmd  of Cmds.t list
  | Hbox of layout list
  | Vbox of layout list


val plot_pdf : string -> layout -> unit
val plot_sdl : (int * int) option -> layout -> unit
