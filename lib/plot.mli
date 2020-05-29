(** The kind of plots that can be performed. *)
type plot =
  | Heatmap : { data : Heatmap.data; options : Heatmap.options list } -> plot
  | Vector : { data : Vector.data; options : Vector.options list } -> plot
  | Scatter : { data : Scatter.data; options : Scatter.options list } -> plot
  | Graph :
      { data : ('v, 'e) Graph.data; options : ('v, 'e) Graph.options list }
      -> plot

(** A layout is a bunch of plots organized hierarchically in [plotbox]es. *)
type layout =
  | Plot of { vp : Viewport.t; plot : plot }
  | Cmd of Cmds.t list
  | Hbox of layout list
  | Vbox of layout list

val plot_pdf : string -> layout -> unit

val plot_sdl : (int * int) option -> float -> Window.mode -> layout -> unit
