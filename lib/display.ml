open Tsdl
open Gg

(** The output of plotting a basic figure is of type [plot]. *)
type plot = Cmds.layout

type target =
  | Sdl of
      { sdl_window : Sdl.window; win : Window.t; mode : Window.drawing_mode }
  | Pdf of { filename : string }

type window = Tsdl.Sdl.surface

let init_timer () =
  match Sdl.init Sdl.Init.timer with
  | Error (`Msg msg) ->
      Sdl.log "Timer init error: %s" msg ;
      exit 1
  | Ok () -> ()

let init_video () =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg msg) ->
      Sdl.log "Video init error: %s" msg ;
      exit 1
  | Ok () -> ()

let init_sdl ~spec ~mode =
  let window = Window.create V2.zero spec in
  let w = window.xres in
  let h = window.yres in
  init_timer () ;
  init_video () ;
  match Sdl.create_window ~w ~h "SDL OpenGL" Sdl.Window.opengl with
  | Error (`Msg msg) ->
      Sdl.log "Sdl.create_window error: %s" msg ;
      exit 1
  | Ok sdl_window ->
      Sdl.set_window_position sdl_window ~x:10 ~y:10 ;
      Sdl { sdl_window; win = window; mode }

let init_pdf filename = Pdf { filename }

let process_plot ~xmargin ~ymargin ~plot =
  let (cmd, bbox) = Cmds.emit_commands_with_bbox plot in
  let w = xmargin +. Cmds.Bbox.width bbox
  and h = ymargin +. Cmds.Bbox.height bbox in
  (Cmds.center_to_page (w, h) cmd, w, h)

let _render_layout_centered ctx xmargin ymargin layout =
  let (cmd, bbox) = Cmds.emit_commands_with_bbox layout in
  let w = xmargin +. Cmds.Bbox.width bbox
  and h = ymargin +. Cmds.Bbox.height bbox in
  let cmds = Cmds.center_to_page (w, h) cmd in
  Cairo.set_matrix
    ctx
    Cairo.{ xx = 1.0; yx = 0.0; xy = 0.0; yy = ~-.1.0; x0 = 0.0; y0 = h } ;
  Backend.render ctx cmds

let mm_to_pt_ratio = 2.83465

let display_sdl (sdl_window : Sdl.window) (window : Window.t)
    (mode : Window.drawing_mode) layout =
  let (screen_xpos, screen_ypos) = Sdl.get_window_position sdl_window in
  Sdl.set_window_size sdl_window ~w:window.xres ~h:window.yres ;
  (* Reset window position where it was in case resizing moved it. *)
  Sdl.set_window_position sdl_window ~x:screen_xpos ~y:screen_ypos ;
  let window_surface =
    match Sdl.get_window_surface sdl_window with
    | Error (`Msg msg) ->
        Sdl.log "Could not get window surface: %s" msg ;
        exit 1
    | Ok surface -> surface
  in
  (* Lock SDL surface *)
  ( match Sdl.lock_surface window_surface with
  | Error (`Msg msg) ->
      Sdl.log "Could not lock window_surface surface: %s" msg ;
      exit 1
  | Ok () -> () ) ;
  (* 2. Get SDL surface pixels as a bigarray *)
  let sdl_pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim sdl_pixels = window.xres * window.yres) ;
  (* TODO: fix this. *)
  Bigarray.Array1.fill sdl_pixels 0xFFFFFFFFl ;
  let sdl_pixels =
    try
      let genarray = Bigarray.genarray_of_array1 sdl_pixels in
      Bigarray.reshape_2 genarray window.yres window.xres
      (* screen_xsize screen_ysize *)
    with _ ->
      let len = Bigarray.Array1.dim sdl_pixels in
      Sdl.log
        "Error while reshaping pixel array of length %d to screen size %d x %d"
        len
        window.xres
        window.yres ;
      exit 1
  in
  (* Create a Cairo surface to write on the pixels *)
  let sdl_surface =
    Cairo.Image.create_for_data32 ~w:window.xres ~h:window.yres sdl_pixels
  in
  (* draw to [cairo_surface] *)
  let () =
    (* Create Cairo context from Cairo surface *)
    let ctx = Cairo.create sdl_surface in
    (* Set basic drawing parameters. *)
    let (commands, bbox) = Cmds.emit_commands_with_bbox layout in
    let placement_matrix = Window.figure_placement window mode bbox in
    let window_to_device = window.to_device_space in
    (* The order in this multiplication is morally wrong but it works.
       What we need is proper 2d homogeneous coordinates. *)
    let m = M3.mul window_to_device placement_matrix in
    let matrix =
      { Cairo.xx = M3.e00 m;
        xy = 0.0;
        yx = 0.0;
        yy = M3.e11 m;
        x0 = M3.e02 m;
        y0 = M3.e12 m
      }
    in
    Cairo.set_matrix ctx matrix ;
    Cairo.set_line_width ctx 1.0 ;
    Cairo.select_font_face ctx "fixed" ~slant:Upright ~weight:Normal ;
    (* Render layout *)
    Backend.render ctx commands
  in
  Cairo.Surface.finish sdl_surface ;
  (* Unlock SDL surface *)
  Sdl.unlock_surface window_surface ;
  (* Update window. *)
  match Sdl.update_window_surface sdl_window with
  | Error (`Msg msg) ->
      Sdl.log "Could not update window surface: %s" msg ;
      exit 1
  | Ok () -> ()

let sdl_loop sdl_window window mode plot =
  let exception Quit in
  let event = Sdl.Event.create () in
  try
    while true do
      display_sdl sdl_window window mode (plot ()) ;
      while Sdl.poll_event (Some event) do
        let event_type = Sdl.Event.get event Sdl.Event.typ in
        if event_type = Sdl.Event.key_down then raise Quit else ()
      done
    done
  with Quit -> ()

let display_pdf filename plot =
  let (plot, w, h) = process_plot ~xmargin:1.0 ~ymargin:1.0 ~plot in
  (* TODO: make margins a parameter *)
  let w = mm_to_pt_ratio *. w in
  let h = mm_to_pt_ratio *. h in
  let cairo_surface = Cairo.PDF.create filename ~w ~h in
  let ctx = Cairo.create cairo_surface in
  let () =
    Cairo.set_matrix
      ctx
      Cairo.
        { xx = mm_to_pt_ratio;
          yx = 0.0;
          xy = 0.0;
          yy = ~-.mm_to_pt_ratio;
          x0 = 0.0;
          y0 = h
        } ;
    Cairo.set_line_width ctx 0.2 ;
    Cairo.select_font_face ctx "fixed" ~slant:Cairo.Upright ~weight:Cairo.Normal
  in
  List.iter (Backend.render ctx) [plot] ;
  Cairo.Surface.finish cairo_surface

let display ~target ~plot =
  match target with
  | Sdl { sdl_window; win; mode } -> sdl_loop sdl_window win mode plot
  | Pdf { filename } -> display_pdf filename (plot ())

let display_sdl_loop ~target ~sdl_loop =
  match target with
  | Sdl { sdl_window; win; mode } -> sdl_loop sdl_window win mode
  | Pdf _ -> assert false
