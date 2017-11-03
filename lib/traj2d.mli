type traj =
  {
    time : float array;
    xs   : Plot.vector;
    ys   : Plot.vector;
    sty  : Vlayout.Style.t option;
    lbl  : string option
  }

val draw_trajectory : traj:(Vlayout.Pt.t array) -> style:Vlayout.Style.t -> Plot.Commands.t list
val draw_trajectory_arrow : traj:(Vlayout.Pt.t array) -> style:Vlayout.Style.t -> Plot.Commands.t list    

class t : ?xsize:int -> ?ysize:int -> unit ->
  object

    method xlabel_to_tick : float
    method set_xlabel_to_tick : float -> unit

    method ylabel_to_tick : float
    method set_ylabel_to_tick : float -> unit

    method tick_length : float
    method set_tick_length : float -> unit

    method xticks : int
    method set_xticks : int -> unit

    method yticks : int
    method set_yticks : int -> unit
                            
    method text_size : float
    method set_text_size : float -> unit
                                
    method frame_color : Vlayout.Style.color
    method set_frame_color : Vlayout.Style.color -> unit
                                 
    method background_color : Vlayout.Style.color
    method set_background_color : Vlayout.Style.color -> unit

    method caption : string
    method set_caption : string -> unit

    method dynamic : bool
    method set_dynamic : bool -> unit

    method arrow : bool
    method set_arrow : bool -> unit
    
    method plot : ?decorations:Plot.Commands.t list -> data:(traj list) -> Plot.Commands.layout

  end
