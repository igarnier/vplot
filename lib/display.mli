
(** The output of plotting a basic figure is of type [plot]. *)
type plot = Cmds.layout

type target

type window

val init_sdl : unit -> target

val init_pdf : string -> target

val display : target:target -> plot:plot -> unit
