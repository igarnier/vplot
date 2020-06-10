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

module Color = Vlayout.Color

let custom_vertex _v pos scale acc =
  let cmd =
    Cmds.style
      ~style:
        Style.(
          make
            ~stroke:(solid_stroke ~clr:Color.black)
            ~width:(Some 0.1)
            ~dash:None
            ~fill:
              (Some (simple_vertical_gradient ~clr1:Color.red ~clr2:Color.blue)))
      (Cmds.circle ~center:pos ~radius:scale)
  in
  cmd :: acc

let options =
  [ `Custom_vertices custom_vertex;
    `Relaxed_length 300.0;
    `Global_position (Gg.P3.v 0.0 0.0 ~-.10.0) ]

let () =
  let (module G) = impl in
  let module SM = Spring_model.Make (G.V) in
  let module Graph_plotter = Graph_display.Make (SM) in
  let campos = P3.v 0.0 0.0 512. in
  let camera =
    Camera.init
      ~position:campos
      ~eyedist:1000.0
      ~radians:0.0
      ~zoom:1.
      ~axis:V3.ox
  in
  let plot =
    let integrator = Graph_plotter.plot_anim impl ~options ~camera ~graph:g in
    fun () -> Cmds.cmd (integrator ())
  in
  let spec =
    Window.(Resolution_and_dpi { xres = 1024; yres = 1024; dpi = 100. })
  in
  let target = Display.init_sdl ~spec ~mode:Window.Scale_to_window in
  Display.display ~target ~plot
