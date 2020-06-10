open Gg

type size = { width : Units.mm; height : Units.mm }

type t =
  { dpi : float; size : size; xres : int; yres : int; to_device_space : M3.t }

type spec =
  | Size_and_dpi of { size : size; dpi : float }
  | Resolution_and_dpi of { xres : int; yres : int; dpi : float }

type drawing_mode = Scale_to_window | Preserve_aspect | Crop

let pp_size fmtr { width; height } =
  Format.fprintf
    fmtr
    "{ width = %a; height = %a}"
    Units.pp_mm
    width
    Units.pp_mm
    height

let pp_matrix fmtr { Cairo.xx; yx; xy; yy; x0; y0 } =
  Format.fprintf
    fmtr
    "@[{ @[xx = %f ; yx = %f ;@;xy = %f ; yy = %f ;@;x0 = %f ; y0 = %f@] }@]"
    xx
    yx
    xy
    yy
    x0
    y0

let pp fmtr { dpi; size; xres; yres; to_device_space } =
  Format.fprintf
    fmtr
    "@[{ @[dpi = %f;@;size = %a;@;xres = %d;@;yres = %d;@;matrix = %a@] }@]"
    dpi
    pp_size
    size
    xres
    yres
    M3.pp
    to_device_space

let res_of_size_and_dpi size dpi =
  let xres = dpi *. (Units.mm_to_inch size.width :> float) in
  let yres = dpi *. (Units.mm_to_inch size.height :> float) in
  (xres, yres)

let create : V2.t -> spec -> t =
 fun origin spec ->
  match spec with
  | Size_and_dpi { size; dpi } ->
      let (xres, yres) = res_of_size_and_dpi size dpi in
      let xx = xres /. (size.width :> float) in
      let yy = yres /. (size.height :> float) in
      let (x0, y0) = V2.to_tuple origin in
      let to_device_space =
        M3.of_cols
          (V3.v xx 0.0 0.0)
          (V3.v 0.0 ~-.yy 0.0)
          (V3.v ~-.x0 (yres -. y0) 1.0)
      in
      { dpi;
        size;
        xres = int_of_float xres;
        yres = int_of_float yres;
        to_device_space
      }
  | Resolution_and_dpi { xres; yres; dpi } ->
      let fxres = float xres in
      let fyres = float yres in
      let width = Units.(inch_to_mm (inch (fxres /. dpi))) in
      let height = Units.(inch_to_mm (inch (fyres /. dpi))) in
      let xx = fxres /. (width :> float) in
      let yy = fyres /. (height :> float) in
      let (x0, y0) = V2.to_tuple origin in
      let to_device_space =
        M3.of_cols
          (V3.v xx 0.0 0.0)
          (V3.v 0.0 ~-.yy 0.0)
          (V3.v ~-.x0 (fyres -. y0) 1.)
      in
      { dpi; size = { width; height }; xres; yres; to_device_space }

let figure_placement : t -> drawing_mode -> Cmds.Bbox.t -> M3.t =
 fun window mode bbox ->
  match mode with
  | Scale_to_window ->
      let ww = (window.size.width :> float) in
      let wh = (window.size.height :> float) in
      let pw = Cmds.Bbox.width bbox in
      let ph = Cmds.Bbox.height bbox in
      let xx = ww /. pw in
      let yy = wh /. ph in
      let (x0, y0) = V2.to_tuple (Cmds.Bbox.sw bbox) in
      let x0 = x0 *. xx in
      let y0 = y0 *. yy in
      M3.of_cols (V3.v xx 0.0 0.0) (V3.v 0.0 yy 0.0) (V3.v ~-.x0 ~-.y0 1.0)
  | Preserve_aspect ->
      let ww = (window.size.width :> float) in
      let wh = (window.size.height :> float) in
      let pw = Cmds.Bbox.width bbox in
      let ph = Cmds.Bbox.height bbox in
      let xx = ww /. pw in
      let yy = wh /. ph in
      let (x0, y0) = V2.to_tuple (Cmds.Bbox.sw bbox) in
      let x0 = x0 *. xx in
      let y0 = y0 *. yy in
      (* select scaling dimension closest to 1 *)
      let s =
        if abs_float (xx -. 1.0) < abs_float (yy -. 1.0) then xx else yy
      in
      M3.of_cols (V3.v s 0.0 0.0) (V3.v 0.0 s 0.0) (V3.v ~-.x0 ~-.y0 1.0)
  | Crop -> M3.id
