open Tsdl

(** The output of plotting a basic figure is of type [plot]. *)
type plot = Cmds.layout


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
  let default_width = 100
  and default_height = 100 in
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
  let (cmds, bbox) = Cmds.emit_commands_with_bbox plot in
  let w    = xmargin +. (Cmds.Bbox.width bbox)
  and h    = ymargin +. (Cmds.Bbox.height bbox) in
  (Cmds.center_to_page (w, h) cmds, w, h)


module Backend = Vlayout.Backends.Cairo(Cmds)

let _render_layout_centered ctx xmargin ymargin layout =
  let (cmds, bbox) = Cmds.emit_commands_with_bbox layout in
  let w    = xmargin +. (Cmds.Bbox.width bbox)
  and h    = ymargin +. (Cmds.Bbox.height bbox) in
  let cmds = Cmds.center_to_page (w, h) cmds in
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

let mm_to_pt_ratio = 2.83465

let display_sdl window plot =
  let (screen_xpos,  screen_ypos)  = Sdl.get_window_position window in
  let (screen_xsize, screen_ysize) = Sdl.get_window_size window in
  (* 0. resize SDL window to plot size, if needed and get surface *)
  let plot, w, h = process_plot 1.0 1.0 plot in (* TODO: make margins a parameter *)
  (* TODO: we should be able to get the PPI from the target display and scale the figure appropriately. *)
  let aspect     = w /. h in
  let _ =
    Sdl.get_display_dpi
  in
  let iw = 800 in
  let ih = int_of_float ((float iw) /. aspect) in
  let sc = (float iw) /. w in
  (* let iw = int_of_float (ceil width) in *)
  (* let ih = int_of_float (ceil height) in *)
  let width, height =
    if screen_xsize < iw || screen_ysize < ih then
      (Sdl.set_window_size window ~w:iw ~h:ih;
       Sdl.set_window_position window ~x:screen_xpos ~y:screen_ypos;
       iw,ih)
    else screen_xsize, screen_ysize
  in
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
  let _ = Bigarray.Array1.fill sdl_pixels 0xFFFFFFFFl in
  (* 3. reshape bigarray to match Cairo's expectations *)
  let sdl_pixels =
    try
      let genarray = Bigarray.genarray_of_array1 sdl_pixels in
      Bigarray.reshape_2 genarray width height (* screen_xsize screen_ysize *)
    with
    | _ ->
      let len = Bigarray.Array1.dim sdl_pixels in
      let err_string = Printf.sprintf "Error while reshaping pixel array of length %d to screen size %d x %d" len width height in
      (Sdl.log "%s" err_string; exit 1)
  in
  (* 4. Create a Cairo surface to write on the pixels *)
  let cairo_surface =
    Cairo.Image.create_for_data32 ~w:width ~h:height sdl_pixels
  in
  (* 5. Create Cairo context from Cairo surface *)
  let ctx = Cairo.create cairo_surface
  in
  (* 6. Set basic drawing parameters. *)
  let _ =
    Cairo.set_matrix
      ctx
      Cairo.({
          xx = sc ; yx = 0.0 ;
          xy = 0.0 ; yy = ~-. sc ;
          x0 = 0.0 ; y0 = (float ih)
        });
    Cairo.set_line_width ctx 1.0;
    Cairo.select_font_face
      ctx
      "fixed"
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

let sdl_loop window plot =
  let exception Quit in
  let event = Sdl.Event.create () in
  try
    while true do
      display_sdl window (plot ());
      while Sdl.poll_event (Some event) do
        let event_type = Sdl.Event.get event Sdl.Event.typ in
        if event_type = Sdl.Event.key_down then
          raise Quit
        else ()
      done
    done
  with
  | Quit -> ()

let display_pdf filename plot =
  let plot, w, h    = process_plot 1.0 1.0 plot in (* TODO: make margins a parameter *)
  let w             = mm_to_pt_ratio *. w in
  let h             = mm_to_pt_ratio *. h in
  let cairo_surface = Cairo.PDF.create filename ~w ~h in
  let ctx           = Cairo.create cairo_surface in
  let _ =
    Cairo.set_matrix
      ctx
      Cairo.({
          xx = mm_to_pt_ratio ; yx = 0.0 ;
          xy = 0.0 ; yy = ~-. mm_to_pt_ratio;
          x0 = 0.0 ; y0 = h
        });
    Cairo.set_line_width ctx 0.2;
    Cairo.select_font_face
      ctx
      "fixed"
      ~slant:Cairo.Upright
      ~weight:Cairo.Normal
  in
  (* 7. Render layout *)
  List.iter (Backend.render ctx) plot;
  Cairo.Surface.finish cairo_surface

let display ~target ~plot =
  match target with
  | Sdl { window }   -> sdl_loop window plot
  | Pdf { filename } -> display_pdf filename (plot ())
