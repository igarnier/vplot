
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

let rec to_vlayout layout =
  match layout with
  | Plot { vp; plot } ->
    let cmd =
      match plot with
      | Heatmap { data; options } ->
        Heatmap.plot ~options ~viewport:vp ~data
      | Vector { data; options } ->
        Vector.plot ~options ~viewport:vp ~data
      | Scatter { data; options } ->
        Scatter.plot ~options ~viewport:vp ~data
    in
    Cmds.cmd [cmd]
  | Cmd cmds ->
    Cmds.cmd cmds
  | Hbox layout_list ->
    let layout_list = List.map to_vlayout layout_list in
    Cmds.hbox ~deltax:5.0 layout_list
  | Vbox layout_list ->
    let layout_list = List.map to_vlayout layout_list in
    Cmds.vbox ~deltay:5.0 layout_list

let plot_pdf filename layout =
  let layout = to_vlayout layout in
  let target = Display.init_pdf filename in
  Display.display ~target ~plot:(fun () -> layout)

let plot_sdl layout =
  let layout = to_vlayout layout in
  let target = Display.init_sdl () in
  Display.display ~target ~plot:(fun () -> layout)
