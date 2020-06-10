open Vlayout (* for Pt *)

type points = Pt.t array

type shape =
  | Dot of { radius : float }
  | Circle of { radius : float }
  | Square of { length : float }
  | Cross of { length : float }
  | Plus of { length : float }
  | Stuff of Cmds.t list

type plot_type =
  | Scatter of { shape : shape; color : Color.t }
  | Traj of traj
  | Decoration of Cmds.t list

and traj = Plain of { sty : Style.t option } | Timed of { time : float array }

type options = Frame.options

type datum = { data : points; plot_type : plot_type }

type data = datum list

(* When doing 2d scatter plots, it is painful to have to decide which style to use for which set of points.
   This function enumerates styles. *)
let enum_shape =
  let shape_of i radius =
    let length = radius *. 2.0 in
    match i mod 5 with
    | 0 -> Dot { radius }
    | 1 -> Circle { radius }
    | 2 -> Square { length }
    | 3 -> Cross { length }
    | 4 -> Plus { length }
    | _ -> failwith "impossible case"
  in
  let r = ref 0.03 in
  let list =
    List.init 5 (fun i ->
        r := !r +. 0.02 ;
        shape_of i !r)
  in
  let state1 = ref list in
  let state2 = ref [] in
  fun () ->
    ( match !state1 with
    | [] ->
        state1 := List.rev !state2 ;
        state2 := []
    | _ -> () ) ;
    match !state1 with
    | hd :: tl ->
        let res = hd in
        state1 := tl ;
        state2 := hd :: !state2 ;
        res
    | _ -> assert false

let enum_scatter () =
  let color = Color.enum_colors () in
  let shape = enum_shape () in
  Scatter { shape; color }

let plot_scatter shape color data =
  let cmds =
    match shape with
    | Dot { radius } ->
        let circle = Cmds.circle ~center:Pt.zero ~radius in
        let style =
          Style.(
            make
              ~stroke:(Solid { c = color })
              ~width:None
              ~dash:None
              ~fill:(Some (Solid { c = color })))
        in
        Cmds.style ~style circle
    | Circle { radius } ->
        let circle = Cmds.circle ~center:Pt.zero ~radius in
        let style =
          Style.(
            make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:None)
        in
        Cmds.style ~style circle
    | Square { length } ->
        let hlen = 0.5 *. length in
        let box =
          Cmds.box ~mins:(Pt.pt ~-.hlen ~-.hlen) ~maxs:(Pt.pt hlen hlen)
        in
        let style =
          Style.(
            make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:None)
        in
        Cmds.style ~style box
    | Cross { length } ->
        let hlen = 0.5 *. length in
        let box = Cmds.Bbox.box (Pt.pt ~-.hlen ~-.hlen) (Pt.pt hlen hlen) in
        let p1 = Cmds.Bbox.nw box in
        let p2 = Cmds.Bbox.se box in
        let seg1 = Cmds.segment ~p1 ~p2 in
        let p1 = Cmds.Bbox.sw box in
        let p2 = Cmds.Bbox.ne box in
        let seg2 = Cmds.segment ~p1 ~p2 in
        let style =
          Style.(
            make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:None)
        in
        Cmds.style ~style (Cmds.wrap [seg1; seg2])
    | Plus { length } ->
        let hlen = 0.5 *. length in
        let vseg = Cmds.segment ~p1:(Pt.pt 0.0 ~-.hlen) ~p2:(Pt.pt 0.0 hlen) in
        let hseg = Cmds.segment ~p1:(Pt.pt ~-.hlen 0.0) ~p2:(Pt.pt hlen 0.0) in
        let style =
          Style.(
            make ~stroke:(Solid { c = color }) ~width:None ~dash:None ~fill:None)
        in
        Cmds.style ~style (Cmds.wrap [vseg; hseg])
    | Stuff cmds -> Cmds.wrap cmds
  in
  Array.map (fun pt -> Cmds.translate ~v:pt cmds) data
  |> Array.to_list |> Cmds.wrap

let draw_trajectory_plain data style =
  let acc = ref [] in
  for i = 1 to Array.length data - 1 do
    let p1 = data.(i - 1) in
    let p2 = data.(i) in
    if p1 != p2 then
      let seg = Cmds.segment ~p1 ~p2 in
      acc := seg :: !acc
    else ()
  done ;
  Cmds.style ~style (Cmds.wrap !acc)

let velocity_idx time data i =
  let len = Array.length time in
  if i > 0 && i < len then
    let idt = 1. /. (time.(i) -. time.(i - 1)) in
    Pt.scale (Pt.sub data.(i) data.(i - 1)) idt
  else invalid_arg "velocity_idx: index out of bounds"

let speed_idx time data i = Pt.norm (velocity_idx time data i)

let instantaneous_speeds time data =
  let len = Array.length time - 1 in
  if len = 0 then invalid_arg "instantaneous_speeds: empty dataectory"
  else Array.init len (fun i -> speed_idx time data (i + 1))

let array_min (arr : float array) =
  let res = ref infinity in
  for i = 0 to Array.length arr - 1 do
    if arr.(i) < !res then res := arr.(i)
  done ;
  !res

let array_max (arr : float array) =
  let res = ref neg_infinity in
  for i = 0 to Array.length arr - 1 do
    if arr.(i) > !res then res := arr.(i)
  done ;
  !res

let draw_trajectory_relspeed time data =
  if Array.length time <= 1 then []
  else
    let speeds = instantaneous_speeds time data in
    let min = array_min speeds in
    let max = array_max speeds in
    let delta = max -. min +. 0.01 (* regularise *) in
    let colors =
      (* if delta < 0.01 then *)
      (*   let clr = Vlayout.Style.gray 0.5 in *)
      (*   Array.create (Array.length speeds) clr *)
      (* else *)
      Array.map
        (fun speed ->
          let ratio = (speed -. min) /. delta in
          Color.rgb ratio 0. 0.)
        speeds
    in
    let acc = ref [] in
    for i = 1 to Array.length data - 1 do
      let p1 = data.(i - 1) in
      let p2 = data.(i) in
      if p1 != p2 then
        let seg = Cmds.segment ~p1 ~p2 in
        let elt =
          Cmds.style
            ~style:
              Style.(
                make
                  ~stroke:(solid_stroke ~clr:colors.(i - 1))
                  ~width:None
                  ~dash:None
                  ~fill:None)
            seg
        in
        acc := elt :: !acc
      else ()
    done ;
    !acc

let plot_traj traj data =
  match traj with
  | Plain { sty } ->
      let style =
        match sty with
        | None ->
            (* Default style: black *)
            Style.(
              make
                ~stroke:(Solid { c = Color.black })
                ~width:None
                ~dash:None
                ~fill:None)
        | Some style -> style
      in
      draw_trajectory_plain data style
  | Timed { time } -> Cmds.wrap (draw_trajectory_relspeed time data)

let plot ~(options : options list) ~viewport ~data =
  let frame = Frame.set_options Frame.default options in
  ( match data with
  | [] -> invalid_arg "plot: empty data list on input"
  | _ -> () ) ;
  let plot =
    List.fold_left
      (fun acc { data; plot_type } ->
        match plot_type with
        | Scatter { shape; color } -> plot_scatter shape color data :: acc
        | Traj traj -> plot_traj traj data :: acc
        | Decoration decs -> decs @ acc)
      []
      data
  in
  let plot = Cmds.wrap plot in
  (* Compute extents *)
  let bbox = Cmds.Bbox.of_command plot in
  (* Add a bit of space *)
  let bbox = Cmds.Bbox.scale 1.1 1.1 bbox in
  let mins = Cmds.Bbox.sw bbox in
  let maxs = Cmds.Bbox.ne bbox in
  let xdomain =
    Utils.linspace (Pt.x mins) (Pt.x maxs) frame.Frame.xaxis.tick_num
  in
  let ydomain =
    Utils.linspace (Pt.y mins) (Pt.y maxs) frame.Frame.yaxis.tick_num
  in
  (* Add frame *)
  let (_, framed) = Frame.add_frame frame xdomain ydomain plot in
  (* Map to viewport *)
  Viewport.apply viewport framed

(* let draw_trajectory_arrow ~traj ~style = *)
(*   let points = Array.to_list traj in *)
(*   let cmds = *)
(*     Cmds.Arrow.mk_multisegment_arrow *)
(*       ~style:Cmds.Arrow.mid_style *)
(*       ~points *)
(*   in *)
(*   [Cmds.style ~style ~subcommands:cmds] *)
