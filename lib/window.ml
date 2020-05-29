type size = { width : Units.mm; height : Units.mm }

type t = { ppi : float; size : size option }

type mode = Rescale | Preserve_aspect | Crop

let pp_size fmtr { width; height } =
  Format.fprintf
    fmtr
    "{ width = %a; height = %a}"
    Units.pp_mm
    width
    Units.pp_mm
    height

let transform picture_size window_size mode =
  match mode with
  | Rescale -> (
      match window_size with
      | None ->
          invalid_arg
            "Window.transform: window has no size, Rescale mode invalid"
      | Some ({ width; height } as window_size) ->
          let ww = (width :> float) in
          let wh = (height :> float) in
          let pw = (picture_size.width :> float) in
          let ph = (picture_size.height :> float) in
          let sw = ww /. pw in
          let sh = wh /. ph in
          (sw, sh, window_size) )
  | Preserve_aspect -> (
      match window_size with
      | None ->
          let window_size = picture_size in
          (1.0, 1.0, window_size)
      | Some ({ width; height } as window_size) ->
          let ww = (width :> float) in
          let wh = (height :> float) in
          let pw = (picture_size.width :> float) in
          let ph = (picture_size.height :> float) in
          let sw = ww /. pw in
          let sh = wh /. ph in
          (* select scaling dimension closest to 1 *)
          if abs_float (sw -. 1.0) < abs_float (sh -. 1.0) then
            (sh, sh, window_size)
          else (sw, sw, window_size) )
  | Crop -> (
      match window_size with
      | None ->
          invalid_arg "Window.transform: window has no size, Crop mode invalid"
      | Some window_size -> (1.0, 1.0, window_size) )

let size_to_res size ppi =
  let mm_per_inch = 25.4 in
  let pixels_per_mm = ppi /. mm_per_inch in
  let xres = (size.width :> float) *. pixels_per_mm in
  let yres = (size.height :> float) *. pixels_per_mm in
  (int_of_float xres, int_of_float yres)

let process_plot ~xmargin ~ymargin ~plot =
  let (cmds, bbox) = Cmds.emit_commands_with_bbox plot in
  let w = xmargin +. Cmds.Bbox.width bbox
  and h = ymargin +. Cmds.Bbox.height bbox in
  (Cmds.center_to_page (w, h) cmds, { width = Units.mm w; height = Units.mm h })

let prepare_layout_for_sdl (window : t) (layout : Cmds.layout) (mode : mode) =
  let (layout, picture_size) =
    process_plot ~xmargin:1.0 ~ymargin:1.0 ~plot:layout
  in
  let (w_scale, h_scale, window_size) =
    transform picture_size window.size mode
  in
  let (xres, yres) = size_to_res window_size window.ppi in
  (* Set basic drawing parameters. *)
  let matrix =
    { Cairo.xx = w_scale *. float xres /. (window_size.width :> float);
      yx = 0.0;
      xy = 0.0;
      yy = ~-.h_scale *. float yres /. (window_size.height :> float);
      x0 = 0.0;
      y0 = float yres (* (window_size.height :> float) *)
    }
  in
  (layout, matrix, xres, yres)
