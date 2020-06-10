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

let rec to_vlayout layout =
  match layout with
  | Plot { vp; plot } ->
      let cmd =
        match plot with
        | Heatmap { data; options } -> Heatmap.plot ~options ~viewport:vp ~data
        | Vector { data; options } -> Vector.plot ~options ~viewport:vp ~data
        | Scatter { data; options } -> Scatter.plot ~options ~viewport:vp ~data
        | Graph { data; options } -> (
            match data with
            | Graph.Graph { impl; graph; _ } ->
                let (module G) = impl in
                let module SM = Spring_model.Make (G.V) in
                let module Graph_plotter = Graph.Make (SM) in
                let camera =
                  Camera.init
                    ~position:Gg.P3.o
                    ~eyedist:100.0
                    ~zoom:1.0
                    ~radians:0.0
                    ~axis:Gg.V3.ox
                in
                Graph_plotter.plot impl ~options ~camera ~graph )
      in
      Cmds.cmd cmd
  | Cmd cmds -> Cmds.cmd (Cmds.wrap cmds)
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

let plot_sdl ~spec ~mode layout =
  let layout = to_vlayout layout in
  let target = Display.init_sdl ~spec ~mode in
  Display.display ~target ~plot:(fun () -> layout)
