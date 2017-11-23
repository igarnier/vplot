open Vlayout

type plot_type =
  | LinLin
(*| LogLin
  | LinLog
  | LogLog *)

type 'a axis = {
  label_to_tick : float;
  tick_length   : float;
  tick_num      : int
}

type absolute
type relative

type axis_option =
  [ `Label_to_tick of float
  | `Tick_length of float
  | `Tick_num of int
  ]

let default_xaxis : relative axis = 
  { label_to_tick = (~-. 0.02); tick_length = (~-. 0.02); tick_num = 10 }
let default_yaxis : relative axis = 
  { label_to_tick = (~-. 0.02); tick_length = (~-. 0.02); tick_num = 10 }

type caption_pos = [ `Above | `Below ]

type caption = { caption_pos : caption_pos; caption : string }

type t =
  {
    plot_type      : plot_type;
    color          : Style.color;
    text_size      : float;
    caption        : caption option;
    xaxis          : relative axis;
    yaxis          : relative axis;
  }

type options =
  [ `Plot_type   of plot_type
  | `Color       of Style.color
  | `Text_size   of float
  | `Caption_pos of caption_pos
  | `Caption     of string
  | `Xaxis       of axis_option list
  | `Yaxis       of axis_option list
  ]

let set_axis_option axis (opt : axis_option) =
  match opt with
  | `Label_to_tick label_to_tick -> { axis with label_to_tick }
  | `Tick_length tick_length     -> { axis with tick_length }
  | `Tick_num tick_num           -> { axis with tick_num }

let set_axis_options axis options =
  List.fold_left set_axis_option axis options

let set_option frame (opt : options) =
  match opt with
  | `Plot_type plot_type     -> { frame with plot_type }
  | `Color color             -> { frame with color }
  | `Text_size text_size     -> { frame with text_size }
  | `Caption_pos caption_pos -> 
    let caption =
      (match frame.caption with
       | None -> Some { caption_pos; caption = "" }
       | Some caption -> Some { caption with caption_pos })
    in
    { frame with caption }
  | `Caption caption         ->
    let caption =
      (match frame.caption with
       | None -> Some { caption_pos = `Above; caption }
       | Some cpt -> Some { cpt with caption })
    in
    { frame with caption }    
  | `Xaxis xaxis_opts        -> { frame with xaxis = set_axis_options default_xaxis xaxis_opts }
  | `Yaxis yaxis_opts        -> { frame with yaxis = set_axis_options default_yaxis yaxis_opts }

let set_options frame options =
  List.fold_left set_option frame options

let default = 
  let xaxis = default_xaxis in
  let yaxis = default_yaxis in
  {
    plot_type      = LinLin;
    color          = Style.black;
    (* bg             : Style.color; *)
    text_size      = 10.;
    caption        = None;
    xaxis;
    yaxis
  }

let text pos (glyph_width,glyph_height) text =
  let len = String.length text in
  let w   = (float len) *. glyph_width in
  Cmds.text ~pos ~width:w ~height:glyph_height ~text

let ticked_horizontal_axis origin width (xaxis : absolute axis) glyph_sz tick_labels  =
  let tick_num  = List.length tick_labels in
  let axis      = Cmds.segment ~p1:origin ~p2:Pt.(pt (x origin +. width) (y origin)) in
  let pos_x     = Utils.interpolate (Pt.x origin) (Pt.x origin +. width) tick_num in
  let tick_cmds =
    List.fold_left2 (fun acc xpos xlab ->
        let p1 = Pt.pt xpos (Pt.y origin) in
        let p2 = Pt.pt xpos (Pt.y origin +. xaxis.tick_length) in
        let tick = Cmds.segment ~p1 ~p2 in
        let labl = text ({ pos = p2; relpos = North }) glyph_sz (Common.float_to_string xlab) in
        tick :: labl :: acc
      ) [] pos_x tick_labels
  in
  axis :: tick_cmds

let ticked_vertical_axis origin height (yaxis : absolute axis) glyph_sz tick_labels =
  let tick_num  = List.length tick_labels in
  let axis      = Cmds.segment ~p1:origin ~p2:Pt.(pt (x origin) ((y origin) +. height)) in
  let pos_y     = Utils.interpolate (Pt.y origin) (Pt.y origin +. height) tick_num in
  let tick_cmds =
    List.fold_left2 (fun acc ypos ylab ->
        let p1 = Pt.pt (Pt.x origin) ypos in
        let p2 = Pt.pt (Pt.x origin +. yaxis.tick_length) ypos in
        let tick = Cmds.segment ~p1 ~p2 in
        let labl = text ({ pos = p2; relpos = East }) glyph_sz (Common.float_to_string ylab) in
        tick :: labl :: acc
      ) [] pos_y tick_labels
  in
  axis :: tick_cmds

let subsample_values values n =
  let len = Array.length values in
  if len <= n then
    Array.to_list values
  else
    let indices = Utils.interpolate 0.0 (float (len - 1)) n in
    let indices = List.map int_of_float indices in
    List.map (fun i -> values.(i)) indices

let apply_frame_color frame_color commands =
  Cmds.style
    ~style:Style.(make ~stroke:(solid_stroke ~clr:frame_color) ~width:None ~dash:None ~fill:None)
    ~subcommands:commands

let rescale_axis s ({ label_to_tick; tick_length; tick_num } : relative axis) =
  ({
    label_to_tick = label_to_tick *. s;
    tick_length   = tick_length *. s;
    tick_num
  } : absolute axis)

let compute_glyph_size frame_width frame_height xticks_num yticks_num =
  (* 6 characters ought to be enough to plot any label, with
     at least the width of two characters between each label. *)
  let glyph_width  = frame_width /. (8.0 *. (float xticks_num)) in
  (* We'd like at least the height of one character between each line. *)
  let glyph_height = frame_height /. (2.0 *. (float yticks_num)) in
  (* Finally, we'd like an aspect ratio of 0.43 (arbitrary). *)
  let aspect_ratio = glyph_height /. glyph_width in
  let target       = 1.0 in
  if aspect_ratio >= target then
    (* too high *)
    glyph_width, glyph_height *. (target /. aspect_ratio)
  else
    glyph_width *. (aspect_ratio /. target), glyph_height
  

let add_frame frm xvalues yvalues plot =
  let bbox   = Cmds.Bbox.of_commands plot in
  let w      = Cmds.Bbox.width bbox in
  let h      = Cmds.Bbox.height bbox in
  let xaxis  = rescale_axis w frm.xaxis in
  let yaxis  = rescale_axis h frm.yaxis in
  let tick_length = min xaxis.tick_length yaxis.tick_length in
  let xaxis  = { xaxis with tick_length } in
  let yaxis  = { yaxis with tick_length } in
  let glyphsz = compute_glyph_size w h xaxis.tick_num yaxis.tick_num in
  let origin = Cmds.Bbox.sw bbox in
  let xlbls  = subsample_values xvalues xaxis.tick_num in
  let xaxis  = ticked_horizontal_axis origin w xaxis glyphsz xlbls in
  let ylbls  = subsample_values yvalues yaxis.tick_num in
  let yaxis  = ticked_vertical_axis origin h yaxis glyphsz ylbls in
  let cmds   =
    match frm.caption with
    | None -> xaxis @ yaxis @ plot
    | Some { caption_pos; caption } ->
      let gap = h *. 0.05 in
      let pos =
        match caption_pos with
        | `Above ->
          let p   = Cmds.Bbox.n bbox in
          let p   = Pt.plus p (Pt.pt 0.0 gap) in
          { Cmds.pos = p; relpos = South }
        | `Below ->
          let p = Cmds.Bbox.s bbox in
          let p   = Pt.plus p (Pt.pt 0.0 (~-. gap)) in
          { Cmds.pos = p; relpos = North }
      in
      let lbl = text pos glyphsz caption in
      lbl :: (xaxis @ yaxis @ plot)
  in
  apply_frame_color frm.color cmds   
 
let add_frame_with_options options xvalues yvalues plot =
  let frm    = set_options default options in
  add_frame frm xvalues yvalues plot
