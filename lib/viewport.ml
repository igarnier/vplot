open Batteries

type range = 
  | Interval of { low : float; high : float }
  | Explicit of float array

type axes = { xrange : range; yrange : range }

type t =
  | AutoX     of { ysize : int }
  | AutoY     of { xsize : int }
  | FixedSize of { xsize : int; ysize : int }

let apply vp plot =
  let bbox = Cmds.Bbox.of_commands plot in
  (* enlarge bbox slightly *)
  let bbox = Cmds.Bbox.enlarge 1.1 1.1 bbox in
  (* put a frame around the box *) 
  let plot = 
    let box = Cmds.box ~mins:(Cmds.Bbox.sw bbox) ~maxs:(Cmds.Bbox.ne bbox) in
    let sty = Vlayout.Style.(make ~stroke:(solid_stroke black) ~width:None ~dash:None ~fill:None) in
    let box = Cmds.style ~style:sty ~subcommands:[box] in
    box :: plot
  in
  let w    = Cmds.Bbox.width bbox in
  let h    = Cmds.Bbox.height bbox in
  let xs, ys =
    match vp with
    | FixedSize { xsize; ysize } ->
      let xs   = (float xsize) /. w in
      let ys   = (float ysize) /. h in
      (xs, ys)
    | AutoY { xsize } ->
      let ar   = h /. w in
      let xs   = (float xsize) /. w in
      let ys   = xs *. ar in
      (xs, ys)
    | AutoX { ysize } ->
      let ar   = h /. w in
      let ys   =  (float ysize) /. h in
      let xs   = ys /. ar in
      (xs, ys)      
  in
  let stroke_width = (0.5 /. xs) in
  let cmds = Cmds.translate ~v:(Vlayout.Pt.scale (Cmds.Bbox.sw bbox) (~-. 1.0)) ~subcommands:plot in
  let cmds = Cmds.scale ~xs ~ys ~subcommands:[cmds] in
  Cmds.style
    ~style:Vlayout.Style.(make ~stroke:(solid_stroke ~clr:black) ~width:(Some stroke_width) ~dash:None ~fill:None) 
    ~subcommands:[cmds]