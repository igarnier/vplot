open Vplot

(* Simplest example *)

let viewport =
  Viewport.FixedSize { xsize = Units.mm 150.0; ysize = Units.mm 100.0 }

let cmd =
  Cmds.segment ~p1:(Pt.pt 0.0 0.0) ~p2:(Pt.pt 150.0 100.0)

let ds =
  let open Scatter in
  { data = [| |] ;
    plot_type = Decoration [cmd]
  }

let plot =
  Plot.Plot { vp = viewport;
              plot =
                Scatter {
                  data = [ds];
                  options = []
                } }


let _ = Plot.plot_sdl (Some (800, 800)) plot
