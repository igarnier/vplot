module G = Graph
open Vplot
open Gg

let camera = Camera.init P3.o 100.0 90.0 V3.ox

module I =
struct
  type t = int
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

module GraphImpl =
  G.Persistent.Graph.Concrete(I)

let impl : ('t, 'v, 'e) Graph_sig.impl = (module GraphImpl)

let g =
  let open GraphImpl in
  let g = empty in
  let g = add_vertex g 0 in
  let g = add_vertex g 1 in
  let g = add_vertex g 2 in
  let g = add_edge g 0 1 in
  let g = add_edge g 1 2 in
  let g = add_edge g 2 0 in
  g

let viewport =
  Viewport.FixedSize { xsize = Units.mm 150.0; ysize = Units.mm 150.0 }

let custom_vertex _v pos acc =
  let cmd =
    Cmds.style
      ~style:Style.(make
                      ~stroke:(solid_stroke ~clr:black)
                      ~width:(Some 0.1)
                      ~dash:None
                      ~fill:(Some (simple_vertical_gradient ~clr1:red ~clr2:blue)))
      ~subcommands:[
        Cmds.circle ~center:pos ~radius:1.0
      ] in
  cmd :: acc

let graph_plot =
  Plot.(Plot { vp = viewport ;
               plot = Graph { data = Graph.Graph { impl ; graph = g } ;
                              options = [
                                `Custom_vertices custom_vertex
                              ] }
             }
       )

let _ =
  let camera = Camera.init Gg.P3.o 100.0 90.0 Gg.V3.ox in
  let dims = Some (1024, 1024) in
  let plotter = Graph.plot_anim impl ~options:[`Custom_vertices custom_vertex] ~viewport ~camera ~graph:g in
  let plot () =
    Cmds.cmd [plotter ()] in
  let target = Display.init_sdl ~dims in
  Display.display ~target ~plot
