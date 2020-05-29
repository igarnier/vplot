(* This stuff should maybe happen at the vlayout level. TODO? *)

type mm = float

type cm = float

type pt = float

let mm x = x

let cm x = x

let pt x = x

let mm_to_cm mm = mm *. 0.1

let cm_to_mm cm = cm *. 10.0

let mm_to_pt mm = mm *. 2.83465

let pt_to_mm pt = pt /. 2.83465

let pp_mm fmtr mm = Format.fprintf fmtr "%f mm" mm

let pp_cm fmtr cm = Format.fprintf fmtr "%f cm" cm

let pp_pt fmtr pt = Format.fprintf fmtr "%f pt" pt
