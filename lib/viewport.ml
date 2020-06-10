type t =
  | AutoX of { ysize : Units.mm }
  | AutoY of { xsize : Units.mm }
  | FixedSize of { xsize : Units.mm; ysize : Units.mm }

let apply vp plot =
  let bbox = Cmds.Bbox.of_command plot in
  (* enlarge bbox slightly *)
  let bbox = Cmds.Bbox.enlarge 1.1 1.1 bbox in
  (* put a frame around the box *)
  let plot =
    let box = Cmds.box ~mins:(Cmds.Bbox.sw bbox) ~maxs:(Cmds.Bbox.ne bbox) in
    let sty =
      let open Vlayout.Style in
      make
        ~stroke:(solid_stroke ~clr:Vlayout.Color.black)
        ~width:None
        ~dash:None
        ~fill:None
    in
    let box = Cmds.style ~style:sty box in
    Cmds.wrap [box; plot]
  in
  let w = Cmds.Bbox.width bbox in
  let h = Cmds.Bbox.height bbox in
  let (xs, ys) =
    match vp with
    | FixedSize { xsize; ysize } ->
        let xs = (xsize :> float) /. w in
        let ys = (ysize :> float) /. h in
        (xs, ys)
    | AutoY { xsize } ->
        let ar = h /. w in
        let xs = (xsize :> float) /. w in
        let ys = xs *. ar in
        (xs, ys)
    | AutoX { ysize } ->
        let ar = h /. w in
        let ys = (ysize :> float) /. h in
        let xs = ys /. ar in
        (xs, ys)
  in
  let cmd =
    Cmds.translate ~v:(Vlayout.Pt.scale (Cmds.Bbox.sw bbox) ~-.1.0) plot
  in
  let cmds = Cmds.scale ~xs ~ys cmd in
  Cmds.style
    ~style:
      Vlayout.Style.(
        make
          ~stroke:(solid_stroke ~clr:Vlayout.Color.black)
          ~width:None
          ~dash:None
          ~fill:None)
    cmds
