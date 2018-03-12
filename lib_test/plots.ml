open Vplot

module Pt = Vlayout.Pt

let pi = acos (~-. 1.)


(* Simplest example *)

let viewport =
  Viewport.FixedSize { xsize = Units.mm 150.0; ysize = Units.mm 100.0 }

let cmd =
  Cmds.segment ~p1:(Pt.pt 0.0 0.0) ~p2:(Pt.pt 150.0 100.0)

let ds =
  let open Scatter in
  { data = [||];
    plot_type = Decoration [cmd]
  }

let plot =
  Plot.Plot { vp = viewport;
              plot = Scatter
                {
                  data = [ds];
                  options = []
                }
            }

let _ = Plot.plot_pdf "diagonal.pdf" plot

(* Test heatmap plot *)

let xdomain = Owl.Mat.linspace 0.0 (2. *. pi) 300

let ydomain = Owl.Mat.linspace 0.0 (2. *. pi) 300

let f x y = (cos x) *. (cos y)

let heatmap =
  Plot.Plot { vp   = Viewport.AutoY { xsize = Units.mm 150.0 };
              plot = Heatmap { data = Heatmap.Fun { xdomain; ydomain; f };
                               options = [ `Blocksize (3,3);
                                           `Axis [`Tick_length (Units.mm 5.0) ];
                                           `Caption "f(x,y) = cos(x) cos(y)"
                                         ] }
            }

let _ = Plot.plot_pdf "heatmap.pdf"  heatmap

(* Test vector plot *)

let viewport =
  Viewport.FixedSize { xsize = Units.mm 150.0; ysize = Units.mm 100.0 }

let domain = Owl.Mat.linspace 0.0 (3. *. pi) 150

let vec1 = Owl.Mat.cos domain
(* let vec1 = Owl.Vec.mul_scalar vec1 10.0 *)

let vec2 = Owl.Mat.sin domain

let vector =
  Plot.Plot { vp   = viewport;
              plot = Vector { data = { domain;
                                       vecs = [Simple vec1;
                                               Full { data = vec2;
                                                      sty = Vlayout.Style.(make ~stroke:(solid_stroke ~clr:green) ~width:None ~dash:(Some dot_dash) ~fill:None);
                                                      lab = "" }] };
                              options = [] }
            }

let _ = Plot.plot_pdf "vector.pdf" vector

(* Test scatter plot *)

let viewport =
  Viewport.AutoY { xsize = Units.mm 150.0 }

let mean0 =
  Owl.Mat.of_arrays [| [| 1.0; ~-. 0.5 |] |]

let mean1 =
  Owl.Mat.of_arrays [| [| 2.5;  2.0 |] |]

let mean2 =
  Owl.Mat.of_arrays [| [| ~-. 1.5;  0.0 |] |]

let vec2pt vec = Vlayout.Pt.pt (Owl.Mat.get vec 0 0) (Owl.Mat.get vec 0 1)

let dataset0 =
  let open Scatter in
  { data = Array.init 60 (fun _ -> vec2pt (Owl.Mat.add mean0 (Owl.Mat.gaussian 1 2)));
    plot_type = Scatter { shape = Circle { radius = 0.05 }; color = Style.black }
  }

let dataset1 =
  let open Scatter in
  { data = Array.init 60 (fun _ -> vec2pt (Owl.Mat.add mean1 (Owl.Mat.gaussian 1 2)));
    plot_type = Scatter { shape = Square { length = 0.05 }; color = Style.red }
  }

let dataset2 =
  let open Scatter in
  { data = Array.init 60 (fun _ -> vec2pt (Owl.Mat.add mean2 (Owl.Mat.gaussian 1 2)));
    plot_type = Scatter { shape = Cross { length = 0.05 }; color = Style.green }
  }

let scatter =
  Plot.Plot { vp   = viewport;
              plot = Scatter { data = [ dataset0; dataset1; dataset2 ];
                               options = []
                             }
            }

let _ = Plot.plot_pdf "scatter.pdf" scatter

(* Test tree *)
let tree = `Node(10, [`Node(2, [`Node(0, []); `Node(1, [])]); `Node(2, [`Node(0, []); `Node(1, [])])])

let plot =
  Trees.plot
    ~options:[]
    ~viewport
    ~data:{ tree; 
            tree_type = Trees.Dendrogram
                { 
                  lbl = (fun i -> 
                      [ Cmds.text
                          ~pos:{ pos = Vlayout.Pt.zero; relpos = Cmds.North } 
                          ~size:10.0
                          ~text:(string_of_int i)
                      ], 10.0
                    )
                }
          }

let tree = Plot.Cmd [plot]

let _ = Plot.plot_pdf "tree.pdf" tree


let all_together =
  Plot.Vbox [
    heatmap;
    Plot.Hbox [ vector; scatter ];
    tree
  ]

let _ = Plot.plot_pdf "all.pdf" all_together
