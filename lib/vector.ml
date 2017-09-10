open Vlayout (* for Pt *)

module Log = Log.Make(struct let section = "basic" end)

let extract_ticks vec ticks =
  let len = Owl.Vec.numel vec in
  if ticks < 2
  then (Log.error "extract_ticks: not enough ticks on axis."; exit 1);
  if len < ticks
  then (Log.error "extract_ticks: too many ticks on axis."; exit 1);
  let stride = len / (ticks - 1) in
  let acc    = ref [vec.{0,0}] in
  for i = 1 to ticks - 2 do
    acc := vec.{0, i * stride} :: !acc
  done;
  List.rev (vec.{0,len-1} :: !acc)

(* Samples should be selected preferentially where the function has a lot of curvature, 
   which for a triple of consecutive points corresponds to how "unflat" is the angle
   that they form. I.e. if we have data points y_1, y_2, y_3 (and we assume the x_i 
   are equispaced) then we get rid of y_2 if (y_1 - y_2) dot (y_3 - y_2) < threshold (TODO: check)
*)
let subsample data =
  let len = Owl.Vec.numel data in
  let rec loop vprev vnow inow vnext acc =
    if inow = (len-2) then (inow + 1) :: inow :: acc
    else
      let dot   = (vprev -. vnow) *. (vnext -. vnow) in (* TODO: normalise length of these vectors*)
      let iprev = inow
      and vprev = vnow
      and vnow  = vnext
      and inow  = inow + 1
      and vnext = data.{0,inow + 2} in
      if false (*TODO: insert appropriate test against well-chosen bound*) then
        (* Curvature et rid of vnow *)
        loop vprev vnow inow vnext acc
      else
        loop vprev vnow inow vnext (iprev :: acc)
  in
  if len = 0 then []
  else if len = 1 then [0]
  else if len = 2 then [0;1]
  else
    let vprev = data.{0,0}
    and vnow  = data.{0,1}
    and inow  = 1
    and vnext = data.{0,2}
    and acc   = []
    in
    loop vprev vnow inow vnext acc

module Commands = Plot.Commands

type range = { low : float; high : float }

class t ?(xsize=600) ?(ysize=600) () =
  object (self)
    (* Default values for the plot parameters *)
    val mutable frame_color      = Style.gray 0.5
    val mutable background_color = Style.black
    val mutable xlabel_to_tick   = ~-. 10.
    val mutable ylabel_to_tick   = ~-. 10.
    val mutable tick_length      = 10.
    val mutable xticks           = 6
    val mutable yticks           = 6
    val mutable text_size        = 10.
    val mutable caption          = ""
    val mutable dynamic          = false
    (* Internal state, used for dynamic plots *)
    val mutable min_value        = max_float
    val mutable max_value        = ~-. max_float

    (* boilerplate *)
    method xlabel_to_tick = xlabel_to_tick
    method set_xlabel_to_tick f = xlabel_to_tick <- f

    method ylabel_to_tick = ylabel_to_tick
    method set_ylabel_to_tick f = ylabel_to_tick <- f

    method tick_length = tick_length
    method set_tick_length f = tick_length <- f

    method xticks = xticks
    method set_xticks i = xticks <- i

    method yticks = yticks
    method set_yticks i = yticks <- i
                            
    method text_size = text_size
    method set_text_size f = text_size <- f

    method frame_color = frame_color
    method set_frame_color c = frame_color <- c
                                 
    method background_color = background_color
    method set_background_color c = background_color <- c

    method caption = caption
    method set_caption s = caption <- s

    method dynamic = dynamic
    method set_dynamic b = dynamic <- b

    method plot ~(domain:Plot.vector) ~(data:Plot.vector) =
      let domlen = Owl.Vec.numel domain in
      let datlen = Owl.Vec.numel data in
      if domlen <> datlen then
        (Log.error "basic: size mismatch: |domain| = %d, |data| = %d" domlen datlen; exit 1)
      else ();
      let minv, maxv =
        if self#dynamic then
          let minv, maxv = Utils.vector_range data in
          let minv       = Utils.min min_value minv
          and maxv       = Utils.max max_value maxv in
          min_value <- minv;
          max_value <- maxv;
          minv, maxv
        else
          Utils.vector_range data
      in
      let deltay       = maxv -. minv in
      let yratio       = (float ysize) /. deltay in
      let scaled_data  = 
        data
        |> (fun v -> Owl.Vec.sub_scalar v minv)
        |> (fun v -> Owl.Vec.mul_scalar v yratio)
      in
      let samples      = subsample scaled_data in
      match List.rev samples with
      | [] | [_] ->
        (Log.fatal "basic: sample list too short. Bug found."; exit 0)
      | index :: tl ->
        let v      = scaled_data.{0,index} in
        let pt     = Pt.pt (float 0) v in
        let xratio = (float xsize) /. (float datlen) in
        let segments, _ =
          List.fold_left (fun (segments, prevpt) index ->
              let v'  = scaled_data.{0,index} in
              let pt  = Pt.pt (xratio *. (float index)) v' in
              let seg = Commands.segment ~p1:prevpt ~p2:pt in
              (seg :: segments, pt)
            ) ([], pt) tl
        in
        let curve  =
          Commands.([style
                       ~style:Style.(make ~stroke:(solid_stroke ~clr:red) ~fill:None)
                       ~subcommands:segments])
        in
        let ticks_x = extract_ticks domain self#xticks in
        let ticks_y = Utils.interpolate minv maxv self#yticks in
        let vertical_axis   =
          Common.ticked_vertical_axis
            ~tick_text_size:self#text_size
            ~label_to_tick:self#ylabel_to_tick
            ~tick_length:self#tick_length
            ~origin:Pt.zero
            ~length:(float ysize)
            ~tick_labels:ticks_y
        in
        let horizontal_axis =
          Common.ticked_horizontal_axis
            ~tick_text_size:self#text_size
            ~label_to_tick:self#xlabel_to_tick
            ~tick_length:self#tick_length
            ~origin:Pt.zero
            ~length:(float xsize)
            ~tick_labels:ticks_x
        in
        Common.(
          apply_frame_color self#frame_color (curve @ vertical_axis @ horizontal_axis)
          |> command
          |> apply_label_below self#caption self#text_size self#frame_color 30. 
        )


  end
