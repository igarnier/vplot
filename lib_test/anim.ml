open Vplot
module G = Graph
open Gg

module I = struct
  type t = int

  let compare = compare

  let equal = ( = )

  let hash = Hashtbl.hash
end

module GraphImpl = G.Persistent.Graph.Concrete (I)

let impl : ('t, 'v, 'e) Graph_sig.impl = (module GraphImpl)

let g =
  let open GraphImpl in
  let g = empty in
  let g = add_vertex g 0 in
  let g = add_vertex g 1 in
  let g = add_vertex g 2 in
  let g = add_vertex g 3 in
  let g = add_edge g 0 1 in
  let g = add_edge g 1 2 in
  let g = add_edge g 2 3 in
  let g = add_edge g 3 0 in
  g

let viewport =
  Viewport.FixedSize { xsize = Units.mm 150.0; ysize = Units.mm 150.0 }

let custom_vertex _v pos acc =
  let cmd =
    Cmds.style
      ~style:
        Style.(
          make
            ~stroke:(solid_stroke ~clr:black)
            ~width:(Some 0.1)
            ~dash:None
            ~fill:(Some (simple_vertical_gradient ~clr1:red ~clr2:blue)))
      ~subcommands:[Cmds.circle ~center:pos ~radius:1.0]
  in
  cmd :: acc

let options =
  [ `Custom_vertices custom_vertex;
    `Relaxed_length 300.0;
    `Global_position (Gg.P3.v 0.0 0.0 ~-.10.0) ]

let () =
  let dims = Some (1024, 1024) in
  let (module G) = impl in
  let module SM = Spring_model.Make (G.V) in
  let module Graph_plotter = Graph_display.Make (SM) in
  let camera =
    Camera.init
      ~position:P3.(v 0.0 0.0 0.0)
      ~eyedist:100.0
      ~radians:(acos ~-.1.0)
      ~zoom:1.
      ~axis:V3.ox
  in
  let plot =
    let integrator = Graph_plotter.plot_anim impl ~options ~camera ~graph:g in
    fun () -> Cmds.cmd [integrator ()]
  in
  let target = Display.init_sdl ~dims ~dpi:100. ~mode:Window.Rescale in
  Display.display ~target ~plot
