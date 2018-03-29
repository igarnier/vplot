open Vlayout

type plot_type =
  | LinLin
(*| LogLin
  | LinLog
  | LogLog *)

type axis = {
  label_to_tick : Units.mm;
  tick_length   : Units.mm;
  tick_num      : int;
  axis_label    : string
}

type axis_option =
  [ `Label_to_tick of Units.mm
  | `Tick_length of Units.mm
  | `Tick_num of int
  | `Label of string
  ]

type caption_pos = [ `Above | `Below ]

type caption = { caption_pos : caption_pos; caption : string }

type t =
  {
    plot_type      : plot_type;
    color          : Style.color;
    text_size      : Units.pt;
    caption        : caption option;    
    xaxis          : axis;
    yaxis          : axis;
    decorations    : Cmds.t list
  }

type options =
  [ `Plot_type   of plot_type
  | `Color       of Style.color
  | `Text_size   of Units.pt
  | `Caption_pos of caption_pos
  | `Caption     of string
  | `Xaxis       of axis_option list
  | `Yaxis       of axis_option list
  | `Decoration  of Cmds.t list
  ]


(* /!\ magical constants /!\, control the default shape and scale of a plot. *)
let natural_x = Units.mm 150.0
let natural_y = Units.mm 150.0


let default_xaxis = 
  { label_to_tick = Units.mm 2.0; tick_length = Units.mm 5.0; tick_num = 5; axis_label = "" }

let default_yaxis = 
  { label_to_tick = Units.mm 2.0; tick_length = Units.mm 5.0; tick_num = 5; axis_label = "" }


let set_axis_option axis (opt : axis_option) =
  match opt with
  | `Label_to_tick label_to_tick -> { axis with label_to_tick }
  | `Tick_length tick_length     -> { axis with tick_length }
  | `Tick_num tick_num           -> { axis with tick_num }
  | `Label axis_label            -> { axis with axis_label }

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
  | `Decoration cmds         -> { frame with decorations = frame.decorations @ cmds }

let set_options frame options =
  List.fold_left set_option frame options

let default = 
  let xaxis = default_xaxis in
  let yaxis = default_yaxis in
  {
    plot_type      = LinLin;
    color          = Style.black;
    (* bg             : Style.color; *)
    text_size      = Units.pt 5.;
    caption        = None;
    xaxis;
    yaxis;
    decorations    = []
  }

let float_to_string f = Printf.sprintf "%.2f" f
let float_to_string f = Printf.sprintf "%g" f

let text pos (text_size : Units.pt) str =
  Cmds.text ~pos ~size:(text_size:>float) ~text:str

let ticked_horizontal_axis (side : [`Up|`Down]) origin width xaxis text_size tick_labels  =
  let tick_num  = List.length tick_labels in
  let axis      = Cmds.segment ~p1:origin ~p2:Pt.(pt (x origin +. width) (y origin)) in
  let pos_x     = Utils.interpolate (Pt.x origin) (Pt.x origin +. width) tick_num in
  let tick_cmds =
    List.fold_left2 (fun acc xpos xlab ->
        let p1 = Pt.pt xpos (Pt.y origin) in
        let p2 = Pt.pt xpos (Pt.y origin +. (xaxis.tick_length :> float)) in
        let tick = Cmds.segment ~p1 ~p2 in
        let pos  = match side with
          | `Up ->
            let lpos = if Pt.y p2 < Pt.y p1 then p1 else p2 in
            let lpos = Pt.plus lpos (Pt.pt 0.0 (abs_float (xaxis.label_to_tick :> float))) in
            { Cmds.pos = lpos; relpos = South }
          | `Down ->
            let lpos = if Pt.y p2 > Pt.y p1 then p1 else p2 in
            let lpos = Pt.plus lpos (Pt.pt 0.0 (~-. (abs_float (xaxis.label_to_tick :> float)))) in
            { Cmds.pos = lpos; relpos = North }
        in
        (* add a bit of space between the label and its anchor *)
        (* let lpos = Pt.plus lpos (Pt.pt 0.0 (~-. (abs_float (xaxis.label_to_tick :> float)))) in *)
        (* let pos  = { Cmds.pos = lpos; relpos = North } in *)
        let str  = float_to_string xlab in
        let labl = text pos text_size str in
        tick :: labl :: acc
      ) [] pos_x tick_labels
  in
  axis :: tick_cmds

let ticked_vertical_axis (side : [`Left|`Right]) origin height yaxis text_size tick_labels =
  let tick_num  = List.length tick_labels in
  let axis      = Cmds.segment ~p1:origin ~p2:Pt.(pt (x origin) ((y origin) +. height)) in
  let pos_y     = Utils.interpolate (Pt.y origin) (Pt.y origin +. height) tick_num in
  let tick_cmds =
    List.fold_left2 (fun acc ypos ylab ->
        let p1   = Pt.pt (Pt.x origin) ypos in
        let p2   = Pt.pt (Pt.x origin +. (yaxis.tick_length :> float)) ypos in
        let tick = Cmds.segment ~p1 ~p2 in
        let pos  = match side with
          | `Left ->
            let lpos = if Pt.x p2 > Pt.x p1 then p1 else p2 in
            let lpos = Pt.plus lpos (Pt.pt (~-. (abs_float (yaxis.label_to_tick :> float))) 0.0) in
            { Cmds.pos = lpos; relpos = East }
          | `Right ->
            let lpos = if Pt.x p2 < Pt.x p1 then p1 else p2 in
            let lpos = Pt.plus lpos (Pt.pt ((abs_float (yaxis.label_to_tick :> float))) 0.0) in
            { Cmds.pos = lpos; relpos = West }
        in
        (* let lpos = Pt.plus lpos (Pt.pt (~-. (abs_float (yaxis.label_to_tick :> float))) 0.0) in *)
        (* let pos  = { Cmds.pos = lpos; relpos = East } in *)
        let str  = float_to_string ylab in
        let labl = text pos text_size str in
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

let add_frame frm xvalues yvalues plot =
  (* compute bbox and extents of [plot] *)
  let bbox   = Cmds.Bbox.of_commands plot in
  let w      = Cmds.Bbox.width bbox in
  let h      = Cmds.Bbox.height bbox in
  (* Resize [plot] and [bbox] to natural frame *)
  let xscale = (natural_x :> float) /. (w  :> float) in
  let yscale = (natural_y :> float) /. (h  :> float) in
  let plot   = Cmds.scale ~xs:xscale ~ys:yscale ~subcommands:plot in
  let bbox   = Cmds.Bbox.scale xscale yscale bbox in
  let origin = Cmds.Bbox.sw bbox in
  let xlbls  = subsample_values xvalues frm.xaxis.tick_num in
  let xaxis  = ticked_horizontal_axis `Down origin (natural_x :> float) frm.xaxis frm.text_size xlbls in
  (* adding x label *)
  let xaxis =
    if frm.xaxis.axis_label = "" then
      xaxis
    else
      let xlblp  = Pt.((pt 0.0 ((Units.mm (~-. 5.0)) :> float)) + Cmds.Bbox.(s (of_commands xaxis))) in
      let xlbl   =
        Cmds.text
          ~pos:Cmds.({ pos = xlblp; relpos = North }) 
          ~size:(frm.text_size:> float) 
          ~text:frm.xaxis.axis_label 
      in
      xlbl :: xaxis
  in
  (* adding y label *)
  let ylbls = subsample_values yvalues frm.yaxis.tick_num in
  let yaxis = ticked_vertical_axis `Left origin (natural_y :> float) frm.yaxis frm.text_size ylbls in
  let yaxis =
    if frm.yaxis.axis_label = "" then
      yaxis
    else
      let halfpi = 0.5 *. acos (~-. 1.0) in
      let text   = 
        Cmds.text
          ~pos:Cmds.{ pos = Pt.zero; relpos = Absolute } 
          ~size:(frm.text_size:> float)
          ~text:frm.yaxis.axis_label
      in
      let rtext = Cmds.rotate ~radians:halfpi ~subcommands:[text] in
      let ylblp = Pt.((pt (Units.mm (~-. 5.0) :> float) 0.0) + Cmds.Bbox.(w (of_commands yaxis))) in
      let ylbl =
        Cmds.place
          ~pos:Cmds.{ pos = ylblp; relpos = East } 
          ~subcommands:[rtext]
      in
      ylbl :: yaxis
  in
  let commands = xaxis @ yaxis @ [plot] @ frm.decorations in
  let commands =
    match frm.caption with
    | None -> commands
    | Some { caption_pos; caption } ->
      let bbox = Cmds.Bbox.of_commands commands in
      let gap = (Bbox.height bbox) *. 0.1 in
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
      let size = Units.pt (1.5 *. (frm.text_size :> float)) in
      let lbl = Cmds.text ~pos ~size:(size :> float) ~text:caption in
      lbl :: (xaxis @ yaxis @ [plot])
  in
  bbox, apply_frame_color frm.color commands
 
let add_frame_with_options options xvalues yvalues plot =
  let frm = set_options default options in
  add_frame frm xvalues yvalues plot
