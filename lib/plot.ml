open Vlayout
open Tsdl

(* Instantiate the Vlayout.Commands functor *)
module Name =
  struct 
    type t = int

    let compare (x : int) (y : int) =
      if x > y then 1
      else if x = y then 0
      else -1

    let print = string_of_int
                  
  end
    
module Commands = Commands.Make(Name)

module Backend = Backends.Cairo(Commands)


type domain = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

type data1d = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

type data2d = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t
                               
type target =
  | Sdl of { window   : Tsdl.Sdl.window }
  | Pdf of { filename : string }             
                               
type window = Tsdl.Sdl.surface

let init_timer () =
  match Sdl.init Sdl.Init.timer with
  | Error (`Msg msg) -> Sdl.log "Timer init error: %s" msg; exit 1
  | Ok () -> ()

let init_video () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg msg) -> Sdl.log "Video init error: %s" msg; exit 1
  | Ok () -> ()
               
let init_sdl  =
  let default_width = 10
  and default_height = 10 in
  fun () ->
  init_video ();
  init_timer ();
  let window_result =
    Sdl.create_window
      ~w:default_width
      ~h:default_height
      "SDL OpenGL"
      Sdl.Window.opengl
  in
  match window_result with
  | Error (`Msg msg) -> Sdl.log "Create window error: %s" msg; exit 1
  | Ok window ->
     Sdl.set_window_position window ~x:10 ~y:10;
     Sdl { window }

let init_pdf filename = Pdf { filename }
            
let process_plot xmargin ymargin plot =
  let (cmds, bbox) = Commands.emit_commands_with_bbox plot in
  let w    = xmargin +. (Bbox.width bbox)
  and h    = ymargin +. (Bbox.height bbox) in
  (Commands.center_to_page (w, h) cmds, w, h)
    
let render_layout_centered ctx xmargin ymargin layout =
  let (cmds, bbox) = Commands.emit_commands_with_bbox layout in
  let w    = xmargin +. (Bbox.width bbox)
  and h    = ymargin +. (Bbox.height bbox) in
  let cmds = Commands.center_to_page (w, h) cmds in
  let _    =
    Cairo.set_matrix
      ctx
      Cairo.({
                xx = 1.0 ; yx = 0.0 ;
                xy = 0.0 ; yy = ~-. 1.0 ;
                x0 = 0.0 ; y0 = h
    })
  in
  List.iter (Backend.render ctx) cmds
            
let display_sdl window plot =
  let (screen_xsize, screen_ysize) = Sdl.get_window_size window in
  (* 0. resize SDL window to plot size, if needed and get surface *)
  let plot, w, h = process_plot 1.0 1.0 plot in (* TODO: make margins a parameter *)
  let iw = int_of_float (ceil w) in
  let ih = int_of_float (ceil h) in
  if screen_xsize <> iw || screen_ysize <> ih
  then Sdl.set_window_size window ~w:iw ~h:ih
  else ();
  let window_surface =
    match Sdl.get_window_surface window with
    | Error (`Msg msg) -> Sdl.log "Could not get window surface: %s" msg; exit 1    
    | Ok surface -> surface
  in
  (* 1. lock SDL surface *)
  let _ =
    match Sdl.lock_surface window_surface with
    | Error (`Msg msg) ->
       Sdl.log "Could not lock window_surface surface: %s" msg; exit 1    
    | Ok () -> ()
  in
  (* 2. get SDL surface pixels as a bigarray *)
  let sdl_pixels =
    Sdl.get_surface_pixels window_surface Bigarray.int32
  in
  (* TODO: fix this. *)  
  let _ = Bigarray.Array1.fill sdl_pixels 0l in
  (* 3. reshape bigarray to match Cairo's expectations *)
  let sdl_pixels =
    let genarray = Bigarray.genarray_of_array1 sdl_pixels in
    Bigarray.reshape_2 genarray iw ih (* screen_xsize screen_ysize *)
  in
  (* 4. Create a Cairo surface to write on the pixels *)
  let cairo_surface =
    Cairo.Image.create_for_data32 ~width:iw ~height:ih sdl_pixels
  in
  (* 5. Create Cairo context from Cairo surface *)
  let ctx = Cairo.create cairo_surface
  in
  (* 6. Set basic drawing parameters. *)
  let _ =
    Cairo.set_matrix
      ctx
      Cairo.({
                xx = 1.0 ; yx = 0.0 ;
                xy = 0.0 ; yy = ~-. 1.0 ;
                x0 = 0.0 ; y0 = (float ih)
    });
    Cairo.set_line_width ctx 1.0;
    Cairo.select_font_face
      ctx
      "DejaVuSansMono"
      ~slant:Cairo.Upright
      ~weight:Cairo.Normal
  in
  (* 7. Render layout *)
  let _ =
    List.iter (Backend.render ctx) plot;
    (* render_layout_centered ctx 1.0 1.0 plot; *)
    Cairo.Surface.finish cairo_surface
  in
  (* 8. Unlock SDL surface *)
  let _ =
    Sdl.unlock_surface window_surface
  in
  (* 9. Update window. *)
  match Sdl.update_window_surface window with
  | Error (`Msg msg) -> Sdl.log "Could not update window surface: %s" msg; exit 1
  | Ok () -> ()

let display_pdf filename plot =
  let plot, w, h    = process_plot 1.0 1.0 plot in (* TODO: make margins a parameter *)
  let cairo_surface = Cairo.PDF.create ~fname:filename ~width:w ~height:h in  
  let ctx           = Cairo.create cairo_surface in
  let _ =
    Cairo.set_matrix
      ctx
      Cairo.({
                xx = 1.0 ; yx = 0.0 ;
                xy = 0.0 ; yy = ~-. 1.0 ;
                x0 = 0.0 ; y0 = h
    });
    Cairo.set_line_width ctx 1.0;
    Cairo.select_font_face
      ctx
      "DejaVuSansMono"
      ~slant:Cairo.Upright
      ~weight:Cairo.Normal
  in
  (* 7. Render layout *)
  List.iter (Backend.render ctx) plot;
  Cairo.Surface.finish cairo_surface

let display ~target ~plot =
  match target with
  | Sdl { window }   -> display_sdl window plot
  | Pdf { filename } -> display_pdf filename plot
               
