module Log = Log.Make(struct let section = "common" end)
open Plot.Commands
open Vlayout
open Utils
open Batteries


(* let extract_ticks vec ticks = *)
(*   let len = Owl.Vec.numel vec in *)
(*   if ticks < 2 *)
(*   then (Log.error "extract_ticks: not enough ticks on axis."; exit 1); *)
(*   if len < ticks *)
(*   then (Log.error "extract_ticks: too many ticks on axis."; exit 1); *)
(*   let stride = len / (ticks - 1) in *)
(*   let acc    = ref [vec.{0,0}] in *)
(*   for i = 1 to ticks - 2 do *)
(*     acc := vec.{0, i * stride} :: !acc *)
(*   done; *)
(*   List.rev (vec.{0,len-1} :: !acc) *)

let extract_ticks vec ticks =
  let len = Owl.Vec.numel vec in
  if ticks < 2
  then (Log.error "extract_ticks: not enough ticks on axis."; exit 1);
  if len < ticks
  then (Log.error "extract_ticks: too many ticks on axis."; exit 1);
  let stride = len / (ticks - 1) in
  let acc    = ref [Owl.Mat.get vec 0 0] in
  for i = 1 to ticks - 2 do
    acc := (Owl.Mat.get vec 0 (i * stride)) :: !acc
  done;
  List.rev ((Owl.Mat.get vec 0 (len-1)) :: !acc)


let float_to_string f = Printf.sprintf "%.2f" f               

let ticked_vertical_axis ~tick_text_size ~label_to_tick ~tick_length ~origin ~length ~tick_labels =
  let tick_num  = List.length tick_labels in
  let axis      = segment ~p1:origin ~p2:(Pt.(pt origin.x (origin.y +. length))) in
  let pos_y     = Utils.interpolate 0.0 length tick_num in
  let tick_cmds =
    List.fold_left2 (fun acc ypos ylab ->
        let tick = segment
            ~p1:(Pt.(pt origin.x ypos))
            ~p2:(Pt.pt tick_length ypos) in
        let labl = text
            ~pos:({ pos = Pt.pt label_to_tick ypos; relpos = East })
            ~size:tick_text_size
            ~text:(float_to_string ylab)
        in
        tick :: labl :: acc
      ) [] pos_y tick_labels
  in
  axis :: tick_cmds

let ticked_horizontal_axis ~tick_text_size ~label_to_tick ~tick_length ~origin ~length ~tick_labels =
  let tick_num  = List.length tick_labels in
  let axis      = segment ~p1:origin ~p2:(Pt.pt (origin.x +. length) origin.y) in
  let pos_x     = Utils.interpolate 0.0 length tick_num in
  let tick_cmds =
    List.fold_left2 (fun acc xpos xlab ->
        let tick = segment
                     ~p1:(Pt.pt xpos origin.y)
                     ~p2:(Pt.pt xpos tick_length) in
        let labl = text
                     ~pos:({ pos = Pt.pt xpos label_to_tick; relpos = North })
                     ~size:tick_text_size
                     ~text:(float_to_string xlab)
        in
        tick :: labl :: acc
      ) [] pos_x tick_labels
  in
  axis :: tick_cmds

let apply_frame_color frame_color commands =
  style
    ~style:Style.(make ~stroke:(solid_stroke ~clr:frame_color) ~fill:None)
    ~subcommands:commands

let command command =
  cmd ~name:None [ command ]

let label lbl size text_color relpos =
  style
    ~style:(Style.make
              ~stroke:(Vlayout.Style.solid_stroke ~clr:text_color)
              ~fill:None)
    ~subcommands:
      [
        text
          ~pos:{ pos = Pt.zero; relpos }
          ~size
          ~text:lbl
      ]
    

let apply_label_below lbl size text_color deltay commands =
  vbox
    ~deltay
    ~layout_list:
      [ commands; command (label lbl size text_color South) ]


let apply_label_above lbl size text_color deltay commands =
  vbox
    ~deltay
    ~layout_list:
      [ command (label lbl size text_color North); commands ]

