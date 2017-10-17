module Log = Log.Make(struct let section = "traj2d" end)

open Vlayout (* for Pt *)
open Utils
open Batteries    

module Vec = Owl.Vec
    
type traj =
  {
    time : float array;
    xs   : Plot.vector;
    ys   : Plot.vector;
    clr  : Style.color option;
    lbl  : string option        
  }

let extract_ticks vec ticks =
  let len = Owl.Vec.numel vec in
  if ticks < 2
  then (Log.error "extract_ticks: not enough ticks on axis."; exit 1);
  if len < ticks
  then (Log.error "extract_ticks: too many ticks on axis."; exit 1);
  let stride = len / (ticks - 1) in
  let acc    = ref [Owl.Vec.get vec 0] in
  for i = 1 to ticks - 2 do
    acc := (Owl.Vec.get vec (i * stride)) :: !acc
  done;
  List.rev ((Owl.Vec.get vec (len-1)) :: !acc)

module Commands = Plot.Commands

let trajectory_to_points { xs; ys } =
  let size = Vec.numel xs in
  Array.init size (fun i ->
      let x = Vec.get xs i in
      let y = Vec.get ys i in
      Pt.pt x y
    )

let velocity_idx time traj i =
  let len = Array.length time in
  if i > 0 && i < len then
    let idt   = 1. /. (time.(i) -. time.(i-1)) in
    Pt.scale (Pt.minus traj.(i) traj.(i-1)) idt
  else
    raise (Invalid_argument "Traj.speed_idx: index out of bounds")

let speed_idx time traj i =
  Pt.norm (velocity_idx time traj i)

let instantaneous_speeds time traj =
  let len = Array.length time - 1 in
  if len = 0 then
    failwith "instantaneous_speeds: empty trajectory"
  else
    Array.init len (fun i ->
        speed_idx time traj (i+1)
      )

(* let color p = Vlayout.Style.rgb p 0. 0. *)

let draw_trajectory_relspeed time traj =
  if Array.length time <= 1 then []
  else
    let speeds = instantaneous_speeds time traj in
    let min    = Array.min speeds in
    let max    = Array.max speeds in
    let delta  = (max -. min) +. 0.01 (* regularise *) in
    let colors =
      (* if delta < 0.01 then *)
      (*   let clr = Vlayout.Style.gray 0.5 in *)
      (*   Array.create (Array.length speeds) clr *)
      (* else *)
      Array.map (fun speed ->
          let ratio = (speed -. min) /. delta in
          Vlayout.Style.rgb ratio 0. 0.
        ) speeds
    in
    let acc = ref [] in
    for i = 1 to Array.length traj - 1 do 
     let p1   = traj.(i - 1) in
      let p2   = traj.(i) in
      if p1 != p2 then
        let seg  = Commands.segment ~p1 ~p2 in
        let elt  =
          Commands.style
            ~style:Style.(make ~stroke:(solid_stroke ~clr:colors.(i-1)) ~fill:None)
            ~subcommands:[seg]
        in
        acc := elt :: !acc
      else ()
    done;
    !acc

let draw_trajectory_solid time traj clr =
  if Array.length time <= 1 then []
  else
    let acc = ref [] in
    for i = 1 to Array.length traj - 1 do
      let p1   = traj.(i - 1) in
      let p2   = traj.(i) in
      if p1 != p2 then
        let seg  = Commands.segment ~p1 ~p2 in
        acc := seg :: !acc
      else ()
    done;
    [Commands.style
       ~style:Style.(make ~stroke:(solid_stroke ~clr) ~fill:None)
       ~subcommands:!acc]

let transform xsize ysize origin w h =
  let xscale = xsize /. w in
  let yscale = ysize /. h in
  fun pt ->
    let pt = Pt.minus pt origin in
    Pt.pt ((Pt.x pt) *. xscale) ((Pt.y pt) *. yscale)

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

    method plot ?(decorations:Commands.t list option) ~(data:traj list) =
      begin match data with [] -> (Log.error "plot: empty data list on input"; exit 1) | _ -> () end;
      let trajs  = List.map trajectory_to_points data in
      let bboxes = List.map Bbox.of_points_arr trajs in
      let bbox   = List.fold_left Bbox.join Bbox.empty bboxes in
      let transf = transform (float xsize) (float ysize) (Bbox.sw bbox) (Bbox.width bbox) (Bbox.height bbox) in
      let scaled_trajs = List.map (Array.map transf) trajs in
      let commands = List.map2 (fun { time; clr } traj ->
          match clr with
          | None ->
            draw_trajectory_relspeed time traj
          | Some clr ->
            draw_trajectory_solid time traj clr
        ) data scaled_trajs
      in
      let trajectories = List.flatten commands in
      let ticks_x = Utils.interpolate (Pt.x (Bbox.sw bbox)) (Pt.x (Bbox.ne bbox)) self#xticks in
      let ticks_y = Utils.interpolate (Pt.y (Bbox.sw bbox)) (Pt.y (Bbox.ne bbox)) self#yticks in
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
      let commands = List.concat [trajectories; vertical_axis; horizontal_axis] in
      let decorations =
        match decorations with
        | None -> []
        | Some decs -> Commands.map_pt decs transf
      in
      Common.(
        apply_frame_color self#frame_color (commands @ decorations)
        |> command
        |> apply_label_below self#caption self#text_size self#frame_color 30.
      )

  end
