type points = Vlayout.Pt.t array

type shape =
  | Dot    of { radius : float }
  | Circle of { radius : float }
  | Square of { length : float }
  | Cross  of { length : float }
  | Plus   of { length : float }
  | Stuff  of Cmds.t list

type plot_type =
  | Scatter    of { shape : shape; color : Vlayout.Style.color }
  | Traj       of traj
  | Decoration of Cmds.t list

and traj =
  | Plain of { sty : Vlayout.Style.t option }
  | Timed of { time : float array }

type options = Frame.options

type datum = { data : points; plot_type : plot_type }
type data  = datum list

val plot : 
  options:options list ->
  viewport:Viewport.t -> data:data -> Cmds.t
