open Vlayout
open Bigarray
       
module Log = Log.Make(struct let section = "Heatmap" end)
                     
type gradient_spec =
  { 
    gradient_path : Gradient.path; 
    gradient_steps : int 
  }

type palette =
  {
    palette : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
    width   : int
  }

type state = {
  mutable min_value  : float;
  mutable max_value  : float;
}

type hmap_options =
  [ `Gradient of gradient_spec
  | `Blocksize of int * int
  | `State of state
  | `Axis of Frame.axis_option list
  ]

type options = [ hmap_options | Frame.options ]

type data =
  | Mat of { xdomain : Owl.Vec.vec; ydomain : Owl.Vec.vec; mat : Owl.Mat.mat }
  | Fun of { xdomain : Owl.Vec.vec; ydomain : Owl.Vec.vec; f : float -> float -> float }

type hmap_options_rec =
  {
    gradient  : gradient_spec;
    blocksize : int * int;
    state     : state;
    hbar_axis : Frame.relative Frame.axis
  }

let default_state () =
  {
    min_value  = max_float;
    max_value  = ~-. max_float;
  }

let default_blocksize = (1,1)

let default_gradient =
  let gradient_path = 
    [ (Style.black, 0.0); (Style.gray 0.5, 0.5); (Style.red, 1.0) ]
  in
  let gradient_steps = 50 in
  { gradient_path; gradient_steps }

let default_hmap_options () =
  {
    gradient  = default_gradient;
    blocksize = default_blocksize;
    state     = default_state ();
    hbar_axis = 
      { 
        Frame.label_to_tick = 20.; 
        tick_length = 10.; 
        tick_num = 10 
      }
  }

let set_option hmap (opt : hmap_options) =
  match opt with
  | `Gradient gradient   -> { hmap with gradient }
  | `Blocksize blocksize -> { hmap with blocksize }
  | `State state         -> { hmap with state }
  | `Axis hbar_opts      -> 
    { hmap with hbar_axis = Frame.set_axis_options (default_hmap_options ()).hbar_axis hbar_opts }

let parse_options (frame, hmap) (option_list : options list) =
  List.fold_left 
    (fun (frame, hmap) opt ->
       match opt with
       | #Frame.options as o ->
         (Frame.set_option frame o, hmap)
       | #hmap_options as o ->
         (frame, set_option hmap o)
    ) (frame, hmap) option_list

(* Plotting-related functions *)

let clamp (f : float) =
  if f < 0.0 then 0.0
  else if f > 1.0 then 1.0
  else f

let write_data_to_image_noblock { palette; width } minv maxv xlength ylength data =
  let plot_image = Image.create xlength ylength in
  let ilen       = 1.0 /. (maxv -. minv) in
  let width      = float width in
  for i = 0 to xlength - 1 do
    for j = 0 to ylength - 1 do
      let f = Owl.Mat.get data i j in
      let f = clamp ((f -. minv) *. ilen) in
      let c = int_of_float (width *. f) in
      Image.set plot_image i j palette.{c}
    done
  done;
  plot_image
    
let write_data_to_image block_x block_y { palette; width } minv maxv xlength ylength data =
  let ximage     = xlength * block_x
  and yimage     = ylength * block_y in
  let plot_image = Image.create ximage yimage in
  let ilen       = 1.0 /. (maxv -. minv) in
  let width      = float width in
  for i = 0 to xlength - 1 do
    for j = 0 to ylength - 1 do
      let f = Owl.Mat.get data i j in
      let f = clamp ((f -. minv) *. ilen) in
      let c = int_of_float (width *. f) in
      let c = palette.{c} in
      let index_i = i * block_x in
      let index_j = j * block_y in
      for block_i = 0 to block_x - 1 do
        for block_j = 0 to block_y - 1 do
          let i' = index_i + block_i
          and j' = index_j + block_j in
          Image.set plot_image i' j' c
        done
      done
    done
  done;
  plot_image

(* let heatbar minv maxv yticks tick_length ylabel_to_tick orig xlength ylength heatbar_xoffset clr gradient_path = *)
(*   let heatbar_xoffset = 50.0 in *)
(*   let heatbar_width   = 30.0 in *)
(*   let heatbar_xpos    = (Pt.x orig) +. xlength +. heatbar_xoffset in *)
(*   let heatbar_style   = Style.(make *)
(*                                  ~stroke:(solid_stroke ~clr) *)
(*                                  ~fill:(Some (vertical_gradient gradient_path)) *)
(*                                  ~width:None *)
(*                                  ~dash:None *)
(*                               ) *)
(*   in *)
(*   let mins      = Pt.pt heatbar_xpos (Pt.y orig) in *)
(*   let maxs_x    = heatbar_xpos +. heatbar_width in *)
(*   let maxs_y    = (Pt.y orig) +. ylength in *)
(*   let maxs      = Pt.pt maxs_x maxs_y in *)
(*   let bar       = Cmds.style ~style:heatbar_style ~subcommands:[ Cmds.box mins maxs ] in *)
(*   let ticks_y   = Utils.interpolate minv maxv yticks in *)
(*   let pos_y     = Utils.interpolate 0.0 ylength yticks in *)
(*   let bar_ticks = List.fold_left2 (fun acc ypos ylab -> *)
(*       let tick = Cmds.segment *)
(*           ~p1:(Pt.pt maxs_x ypos) *)
(*           ~p2:(Pt.pt (maxs_x -. tick_length) ypos) in *)
(*       let labl = Cmds.text *)
(*           ~pos:({ pos = Pt.pt (maxs_x -. tick_length +. ylabel_to_tick) ypos; relpos = West }) *)
(*           ~width:(0.5 *. xlength) *)
(*           ~height:(0.05 *. xlength) *)
(*           (\* ~size:10.0 *\) *)
(*           ~text:(Common.float_to_string ylab) *)
(*       in *)
(*       tick :: labl :: acc *)
(*     ) [] pos_y ticks_y *)
(*   in *)
(*   let bar_ticks = *)
(*     Cmds.style *)
(*       ~style:Style.(make ~stroke:(solid_stroke ~clr) ~width:None ~fill:None ~dash:None) *)
(*       ~subcommands:bar_ticks *)
(*   in *)
(*   [bar; bar_ticks] *)

let plot_heatmap palette state blocksize xdomain ydomain data =
  let xlength    = Owl.Vec.numel xdomain in  
  let ylength    = Owl.Vec.numel ydomain in
  let xdata      = Owl.Mat.row_num data in
  let ydata      = Owl.Mat.col_num data in
  if xlength <> xdata || ylength <> ydata then
    (Log.error "invalid heatmap size: |xdomain| = %d, |ydomain| = %d, |data| = %dx%d" xlength ylength xdata ydata; exit 1)
  else ();
  let minv, maxv =
    let minv, maxv = Utils.data_range data in
    let minv       = Utils.min state.min_value minv
    and maxv       = Utils.max state.max_value maxv in
    state.min_value <- minv;
    state.max_value <- maxv;
    minv, maxv
  in
  let plot_image =
    match blocksize with
    | (1,1) -> write_data_to_image_noblock palette minv maxv xlength ylength data
    | (i,j) -> write_data_to_image i j palette minv maxv xlength ylength data
  in
  Cmds.image ~pos:(Pt.pt 1.0 1.0) ~image:plot_image

let plot_heatbar width height color { Frame.label_to_tick; tick_length; tick_num } ticks gradient_path =
  let heatbar_style   = Style.(make
                                 ~stroke:(solid_stroke ~clr:color)
                                 ~width:None
                                 ~fill:(Some (vertical_gradient gradient_path))
                                 ~dash:None
                              )
  in
  let mins      = Pt.zero in
  let maxs      = Pt.pt width height in
  let bar       = Cmds.style ~style:heatbar_style ~subcommands:[ Cmds.box mins maxs ] in
  let pos_y     = Utils.interpolate 0.0 height (List.length ticks) in
  let bar_ticks = List.fold_left2 (fun acc ypos ylab ->
      let tick = Cmds.segment
          ~p1:(Pt.pt width ypos)
          ~p2:(Pt.pt (width -. tick_length) ypos) in
      let labl = Cmds.text
          ~pos:({ pos = Pt.pt (width -. tick_length +. label_to_tick) ypos; relpos = West })
          ~width:(0.5 *. width)
          ~height:(0.05 *. width)
          (* ~size:10.0 *)
          ~text:(Common.float_to_string ylab)
      in
      tick :: labl :: acc
    ) [] pos_y ticks
  in
  let bar_ticks =
    Cmds.style
      ~style:Style.(make ~stroke:(solid_stroke ~clr:color) ~width:None ~fill:None ~dash:None)
      ~subcommands:bar_ticks
  in
  [bar; bar_ticks]

let plot_internal frame { gradient; blocksize; state; hbar_axis } =
  let { gradient_path; gradient_steps } = default_gradient in
  let { Gradient.rarr; garr; barr } =
    Gradient.create gradient_path gradient_steps
  in
  let palette =
    let len = Array.length rarr in
    let arr = Array1.create int32 c_layout len in
    for i = 0 to len - 1 do
      arr.{i} <- Style.(to_int (rgb rarr.(i) garr.(i) barr.(i)))
    done;
    { palette = arr; width = len - 1 }
  in
  fun viewport xdomain ydomain data ->
    (* Compute heatmap *)
    let hmap  = plot_heatmap palette state blocksize xdomain ydomain data in
    let bbox  = Cmds.Bbox.of_command hmap in
    (* Apply a frame *)
    let hmap  = Frame.add_frame frame (Owl.Vec.to_array xdomain) (Owl.Vec.to_array ydomain) [hmap] in
    (* Throw in heatbar *)
    let h     = Cmds.Bbox.height bbox in
    let w     = Cmds.Bbox.width bbox in
    let ticks = Utils.interpolate state.min_value state.max_value hbar_axis.Frame.tick_num in
    let hbar  = plot_heatbar (0.2 *. h) h frame.Frame.color hbar_axis ticks gradient_path in
    let hbar  = Cmds.translate ~v:(Pt.pt (w +. (0.1 *. h)) 0.0) ~subcommands:hbar in
    Viewport.apply viewport [hbar; hmap]

(* let plot_mat ~(options : options list) = *)
(*   let frame, hmap_opts = parse_options (Frame.default, default_hmap_options ()) options in *)
(*   let f = plot_internal frame hmap_opts in *)
(*   fun ~viewport ~xdomain ~ydomain ~data -> *)
(*     f viewport xdomain ydomain data *)
      
(* let plot_f ~(options : options list) = *)
(*   let frame, hmap_opts = parse_options (Frame.default, default_hmap_options ()) options in *)
(*   let fc = plot_internal frame hmap_opts in *)
(*   fun ~viewport ~xdomain ~ydomain ~f -> *)
(*   let mat = *)
(*     Owl.Mat.init_nd (Owl.Vec.numel xdomain) (Owl.Vec.numel ydomain) *)
(*       (fun i j -> f (Owl.Vec.get xdomain i) (Owl.Vec.get ydomain j)) *)
(*   in *)
(*   fc viewport xdomain ydomain mat *)

let plot ~(options : options list) =
  let frame, hmap_opts = parse_options (Frame.default, default_hmap_options ()) options in
  let fc = plot_internal frame hmap_opts in
  fun ~viewport ~data  ->
    match data with
    | Mat { xdomain; ydomain; mat } ->
      fc viewport xdomain ydomain mat
    | Fun { xdomain; ydomain; f } ->
      let mat =
        Owl.Mat.init_nd (Owl.Vec.numel xdomain) (Owl.Vec.numel ydomain)
          (fun i j -> f (Owl.Vec.get xdomain i) (Owl.Vec.get ydomain j))
      in
      fc viewport xdomain ydomain mat
