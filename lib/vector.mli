type vec =
  | Full of { data: Plot.vector; clr: Vlayout.Style.color; lab: string }
  | Simple of Plot.vector

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

    method plot : domain:Plot.vector -> data:(vec list) -> Plot.Commands.layout

  end