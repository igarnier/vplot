module Log = Log.Make(struct let section = "traj2d" end)

open Vlayout (* for Pt *)
open Utils
open Batteries    

module Vec = Owl.Vec

type points = Pt.t array

type shape =
  | Dot    of { radius : float }
  | Circle of { radius : float }
  | Square of { length : float }
  | Cross  of { length : float }
  | Plus   of { length : float }
  | Stuff  of Cmds.t list

type plot_type =
  | Scatter    of { shape : shape; color : Style.color }
  | Traj       of traj
  | Decoration of Cmds.t list

and traj =
  | Plain of { sty : Style.t option }
  | Timed of { time : float array }

type options = Frame.options

type datum = { data : points; plot_type : plot_type }
type data  = datum list


let plot_scatter shape color data =
  let cmds =
    match shape with
    | Dot { radius }    ->
      let circle = Cmds.circle ~center:Pt.zero ~radius in
      let style  = Style.(make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:(Some (Solid { c = color }))) in
      [Cmds.style ~style ~subcommands:[circle]]
    | Circle { radius } ->
      let circle = Cmds.circle ~center:Pt.zero ~radius in
      let style  = Style.(make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:None) in
      [Cmds.style ~style ~subcommands:[circle]]
    | Square { length } ->
      let hlen = 0.5 *. length in
      let box  = Cmds.box ~mins:(Pt.pt (~-. hlen) (~-. hlen)) ~maxs:(Pt.pt hlen hlen) in
      let style  = Style.(make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:None) in
      [Cmds.style ~style ~subcommands:[box]]
    | Cross { length } ->
      let hlen = 0.5 *. length in
      let box  = Cmds.Bbox.box (Pt.pt (~-. hlen) (~-. hlen)) (Pt.pt hlen hlen) in
      let p1   = Cmds.Bbox.nw box in
      let p2   = Cmds.Bbox.se box in
      let seg1 = Cmds.segment ~p1 ~p2 in
      let p1   = Cmds.Bbox.sw box in
      let p2   = Cmds.Bbox.ne box in
      let seg2 = Cmds.segment ~p1 ~p2 in
      let style = Style.(make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:None) in
      [Cmds.style ~style ~subcommands:[seg1;seg2]]
    | Plus { length } ->
      let hlen = 0.5 *. length in
      let vseg = Cmds.segment ~p1:(Pt.pt 0.0 (~-. hlen)) ~p2:(Pt.pt 0.0 hlen) in
      let hseg = Cmds.segment ~p1:(Pt.pt (~-. hlen) 0.0) ~p2:(Pt.pt hlen 0.0) in
      let style  = Style.(make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:None) in
      [Cmds.style ~style ~subcommands:[vseg; hseg]]
    | Stuff cmds ->
      cmds
  in
  Array.map (fun pt -> Cmds.translate ~v:pt ~subcommands:cmds) data
  |> Array.to_list


let draw_trajectory_plain data style =
  let acc = ref [] in
  for i = 1 to Array.length data - 1 do
    let p1   = data.(i - 1) in
    let p2   = data.(i) in
    if p1 != p2 then
      let seg  = Cmds.segment ~p1 ~p2 in
      acc := seg :: !acc
    else ()
  done;
  [Cmds.style ~style ~subcommands:!acc]

let velocity_idx time data i =
  let len = Array.length time in
  if i > 0 && i < len then
    let idt   = 1. /. (time.(i) -. time.(i-1)) in
    Pt.scale (Pt.minus data.(i) data.(i-1)) idt
  else
    invalid_arg "velocity_idx: index out of bounds"

let speed_idx time data i =
  Pt.norm (velocity_idx time data i)

let instantaneous_speeds time data =
  let len = Array.length time - 1 in
  if len = 0 then
    invalid_arg "instantaneous_speeds: empty dataectory"
  else
    Array.init len (fun i ->
        speed_idx time data (i+1)
      )

let draw_trajectory_relspeed time data =
  if Array.length time <= 1 then []
  else
    let speeds = instantaneous_speeds time data in
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
    for i = 1 to Array.length data - 1 do 
     let p1   = data.(i - 1) in
      let p2   = data.(i) in
      if p1 != p2 then
        let seg  = Cmds.segment ~p1 ~p2 in
        let elt  =
          Cmds.style
            ~style:Style.(make ~stroke:(solid_stroke ~clr:colors.(i-1)) ~width:None ~dash:None ~fill:None)
            ~subcommands:[seg]
        in
        acc := elt :: !acc
      else ()
    done;
    !acc

let plot_traj traj data =
  match traj with
  | Plain { sty } ->
    let style = match sty with
      | None ->
        (* Default style: black *)
        Style.(make ~stroke:(Solid { c = black }) ~width:None ~dash:None ~fill:None)
      | Some style -> style
    in
    draw_trajectory_plain data style
  | Timed { time } ->
    draw_trajectory_relspeed time data

let plot ~(options : options list) ~viewport ~data =
  let frame = Frame.set_options Frame.default options in
  begin match data with [] -> (Log.error "plot: empty data list on input"; exit 1) | _ -> () end;
  let plot =
    List.fold_left (fun acc { data; plot_type } ->
        match plot_type with
        | Scatter { shape; color } ->
          (plot_scatter shape color data) @ acc
        | Traj traj ->
          (plot_traj traj data) @ acc
        | Decoration decs ->
          decs @ acc
      ) [] data
  in
  (* Compute extents *)
  let bbox = Cmds.Bbox.of_commands plot in
  (* Add a bit of space *)
  let bbox = Cmds.Bbox.scale 1.1 1.1 bbox in
  let mins = Cmds.Bbox.sw bbox in
  let maxs = Cmds.Bbox.ne bbox in
  let xdomain = Utils.linspace (Pt.x mins) (Pt.x maxs) frame.Frame.xaxis.tick_num in
  let ydomain = Utils.linspace (Pt.y mins) (Pt.y maxs) frame.Frame.yaxis.tick_num in
  (* Add frame *)
  let framed = Frame.add_frame frame xdomain ydomain plot in
  (* Map to viewport *)
  Viewport.apply viewport [framed]

(* let draw_trajectory_arrow ~traj ~style = *)
(*   let points = Array.to_list traj in *)
(*   let cmds = *)
(*     Cmds.Arrow.mk_multisegment_arrow *)
(*       ~style:Cmds.Arrow.mid_style *)
(*       ~points *)
(*   in *)
(*   [Cmds.style ~style ~subcommands:cmds] *)
