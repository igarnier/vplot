(** The output of plotting a basic figure is of type [plot]. *)
type plot = Cmds.layout

type target

type window

val init_sdl : spec:Window.spec -> mode:Window.drawing_mode -> target

val init_pdf : string -> target

val display : target:target -> plot:(unit -> plot) -> unit

val display_sdl :
  Tsdl.Sdl.window -> Window.t -> Window.drawing_mode -> Cmds.layout -> unit

val display_sdl_loop :
  target:target ->
  sdl_loop:(Tsdl.Sdl.window -> Window.t -> Window.drawing_mode -> 'a) ->
  'a
