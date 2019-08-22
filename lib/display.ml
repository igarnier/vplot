open Tsdl

(** The output of plotting a basic figure is of type [plot]. *)
type plot = Cmds.layout


type target =
  | Sdl of { window : Tsdl.Sdl.window ; dims : (int * int) option }
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

let init_sdl ~dims =
  init_video ();
  init_timer ();
  let w, h =
    match dims with
    | None -> (100, 100) (* default values, to be overriden automatically *)
    | Some (w, h) -> w, h in
  let window_result =
    Sdl.create_window ~w ~h "SDL OpenGL" Sdl.Window.opengl in
  match window_result with
  | Error (`Msg msg) -> Sdl.log "Create window error: %s" msg; exit 1
  | Ok window ->
     Sdl.set_window_position window ~x:10 ~y:10;
     Sdl { window ; dims }

let init_pdf filename = Pdf { filename }

let process_plot ~xmargin ~ymargin ~plot =
  let (cmds, bbox) = Cmds.emit_commands_with_bbox plot in
  let w = xmargin +. (Cmds.Bbox.width bbox)
  and h = ymargin +. (Cmds.Bbox.height bbox) in
  (Cmds.center_to_page (w, h) cmds, w, h)


module Backend = Vlayout.Backends.Cairo(Cmds)

let _render_layout_centered ctx xmargin ymargin layout =
  let (cmds, bbox) = Cmds.emit_commands_with_bbox layout in
  let w    = xmargin +. (Cmds.Bbox.width bbox)
  and h    = ymargin +. (Cmds.Bbox.height bbox) in
  let cmds = Cmds.center_to_page (w, h) cmds in
  let _    =
    Cairo.set_matrix ctx
      Cairo.({
          xx = 1.0 ; yx = 0.0 ;
          xy = 0.0 ; yy = ~-. 1.0 ;
          x0 = 0.0 ; y0 = h
        }) in
  List.iter (Backend.render ctx) cmds

let mm_to_pt_ratio = 2.83465

let display_sdl window plot dims =
  let (screen_xpos,  screen_ypos)  = Sdl.get_window_position window in
  let (screen_xsize, screen_ysize) = Sdl.get_window_size window in
  (* TODO: make margins a parameter *)
  let plot, nw, nh = process_plot ~xmargin:1.0 ~ymargin:1.0 ~plot in
  (* nw, nh are the natural width and height of the figure.
     If the user explicitly chose dimensions for the window, we
     rescale appropriately. Otherwise, we pick the natural
     dimensions. *)
  (* TODO: we should be able to get the PPI from the target display and scale the figure appropriately. *)
  let w_scale, h_scale, width, height =
    match dims with
    | None ->
       (* User didn't pick any dimensions. Resize window to natural size. *)
       let w = int_of_float nw in
       let h = int_of_float nh in
       Sdl.set_window_size window ~w ~h ;
       (* Ueset window position where it was in case resizing moved it. *)
       Sdl.set_window_position window ~x:screen_xpos ~y:screen_ypos ;
       let w_scale = 1.0 in
       let h_scale = 1.0 in
       w_scale, h_scale, w, h
    | Some (user_w, user_h) ->
       assert (screen_xsize = user_h) ;
       assert (screen_ysize = user_w) ;
       (* User-chosen window dimensons. Compute rescaling factors. *)
       let w = user_w in
       let h = user_h in
       float w /. nw, float h /. nh, user_w, user_h in
  (* let width, height =
   *   if screen_xsize < iw || screen_ysize < ih then
   *     (Sdl.set_window_size window ~w:iw ~h:ih;
   *      Sdl.set_window_position window ~x:screen_xpos ~y:screen_ypos;
   *      iw,ih)
   *   else screen_xsize, screen_ysize
   * in *)
  let window_surface =
    match Sdl.get_window_surface window with
    | Error (`Msg msg) -> Sdl.log "Could not get window surface: %s" msg ; exit 1
    | Ok surface -> surface in
  (* Lock SDL surface *)
  begin match Sdl.lock_surface window_surface with
    | Error (`Msg msg) ->
      Sdl.log "Could not lock window_surface surface: %s" msg ; exit 1
    | Ok () -> ()
  end ;
  (* 2. Get SDL surface pixels as a bigarray *)
  let sdl_pixels = Sdl.get_surface_pixels window_surface Bigarray.int32 in
  assert (Bigarray.Array1.dim sdl_pixels = width * height) ;
  (* TODO: fix this. *)
  let _ = Bigarray.Array1.fill sdl_pixels 0xFFFFFFFFl in
  (* Reshape bigarray to match Cairo's expectations *)
  let sdl_pixels =
    try
      let genarray = Bigarray.genarray_of_array1 sdl_pixels in
      Bigarray.reshape_2 genarray height width (* screen_xsize screen_ysize *)
    with
    | _ ->
      let len = Bigarray.Array1.dim sdl_pixels in
      let err_string =
        Printf.sprintf
          "Error while reshaping pixel array of length %d to screen size %d x %d"
          len width height in
      (Sdl.log "%s" err_string; exit 1) in
  (* Create a Cairo surface to write on the pixels *)
  let cairo_surface =
    Cairo.Image.create_for_data32 ~w:width ~h:height sdl_pixels in
  (* Create Cairo context from Cairo surface *)
  let ctx = Cairo.create cairo_surface in
  (* Set basic drawing parameters. *)
  begin
    let open Cairo in
    let matrix =
      { xx = w_scale ; yx = 0.0;
        xy = 0.0     ; yy = ~-. h_scale ;
        x0 = 0.0     ; y0 = float height } in
    set_matrix ctx matrix ;
    set_line_width ctx 1.0 ;
    select_font_face
      ctx
      "fixed"
      ~slant:Upright
      ~weight:Normal ;
    (* Render layout *)
    List.iter (Backend.render ctx) plot ;
    Surface.finish cairo_surface ;
  end ;
  (* Unlock SDL surface *)
  Sdl.unlock_surface window_surface ;
  (* Update window. *)
  match Sdl.update_window_surface window with
  | Error (`Msg msg) ->
     Sdl.log "Could not update window surface: %s" msg ; exit 1
  | Ok () -> ()

let sdl_loop window plot dims =
  let exception Quit in
  let event = Sdl.Event.create () in
  try
    while true do
      display_sdl window (plot ()) dims;
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
  let plot, w, h    = process_plot ~xmargin:1.0 ~ymargin:1.0 ~plot in (* TODO: make margins a parameter *)
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
  | Sdl { window ; dims }   -> sdl_loop window plot dims
  | Pdf { filename } -> display_pdf filename (plot ())
