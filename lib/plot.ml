open Vlayout
open Tsdl

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

type target =
  | Sdl of { window : Tsdl.Sdl.window }
                               
type window = Tsdl.Sdl.surface

type plot = Commands.layout
                               
                               
type range = { low : float; high : float }

let range_delta { low; high } = high -. low
                               
let interpolate a b n =
  let d = (b -. a) /. (float n) in
  let rec loop x i =
    if i = n then
      [b]
    else
      x :: (loop (x +. d) (i+1))
  in
  loop a 0

let font_width     = 10
let tick_length    = ~-. 10.0
let xlabel_to_tick = 10.0
let ylabel_to_tick = 10.0

let float_to_string f = Printf.sprintf "%.1f" f

let axis ~xrange ~yrange ~xticks ~yticks ~xlength ~ylength ~clr ~data =
  let ticks_x = interpolate xrange.low xrange.high xticks in
  let ticks_y = interpolate yrange.low yrange.high yticks in
  let pos_x   = interpolate 0.0 xlength xticks in
  let pos_y   = interpolate 0.0 ylength yticks in
  (* produce commands *)
  Commands.(
    let orig       = Pt.zero in  
    let x_segment  = segment ~p1:orig ~p2:(Pt.pt xlength 0.0) in
    let y_segment  = segment ~p1:orig ~p2:(Pt.pt 0.0 ylength) in
    let xtick_cmds = List.fold_left2 (fun acc xpos xlab ->
                         let tick = segment
                                     ~p1:(Pt.pt xpos 0.0)
                                     ~p2:(Pt.pt xpos tick_length) in
                         let labl = text
                                      ~pos:({ pos = Pt.pt xpos (~-. xlabel_to_tick); relpos = North })
                                      ~size:10.0
                                      ~text:(float_to_string xlab)
                         in
                         tick :: labl :: acc
                       ) [] pos_x ticks_x
    in
    let ytick_cmds = List.fold_left2 (fun acc ypos ylab ->
                         let tick = segment
                                     ~p1:(Pt.pt 0.0 ypos)
                                     ~p2:(Pt.pt tick_length ypos) in
                         let labl = text
                                      ~pos:({ pos = Pt.pt (~-. ylabel_to_tick) ypos; relpos = East })
                                      ~size:10.0
                                      ~text:(float_to_string ylab)
                         in
                         tick :: labl :: acc
                       ) [] pos_y ticks_y
    in
    let (r, g, b) = clr in
    let clr = color r g b in
    cmd ~name:None
        ([ clr;
           x_segment;
           y_segment ]
         @ xtick_cmds
         @ ytick_cmds
         @ data
        )
  )


(* let plot_image = Image.create 500 500 *)

(* let _ = *)
(*   for i = 0 to 500 - 1 do *)
(*     for j = 0 to 500 - 1 do *)
(*       Image.set plot_image i j 0xFFFF0000l *)
(*     done *)
(*   done *)
          
(* let im  = Commands.image ~pos:Pt.zero ~image:plot_image *)

(* (\* The stuff we want to draw *\)           *)
(* let layout = *)
(*   axis *)
(*     ~xrange:{ low = -. 10.0; high = 10.0 } *)
(*     ~yrange:{ low = -. 10.0; high = 10.0 } *)
(*     ~xticks:6 *)
(*     ~yticks:6 *)
(*     ~xlength:500.0 *)
(*     ~ylength:500.0 *)
(*     ~clr:(1.0, 1.0, 1.0) *)
(*     ~data:[im] *)

(* let layout = *)
(*   Commands.hbox ~deltax:10.0 ~layout_list:[layout; layout] *)

module Heatmap =
  struct

    let pi   = acos (~-. 1.0)
    let tpi  = 2.0 /. pi

    (* atan is bounded by pi/2 *)

    let rgb r g b =
      Int32.(logor (logor (shift_left r 16) (shift_left g 8)) b)

    let interpolate (r1,g1,b1) (r2,g2,b2) f =
      let deltar = float (r2 - r1) in
      let deltag = float (g2 - g1) in
      let deltab = float (b2 - b1) in
      let r      = Int32.of_float (f *. deltar +. (float r1))
      and g      = Int32.of_float (f *. deltag +. (float g1))
      and b      = Int32.of_float (f *. deltab +. (float b1)) in
      rgb r g b

    (* Note that the palette is skewed towards low values. *)
    let palette =
      let palette = Array.make 256 0x00000000l in
      let x1      = 85.0 in
      let x2      = 85.0 *. 2.0 in
      for i = 0 to (int_of_float x1) - 1 do
        let ratio = (float i) /. x1 in
        palette.(i) <- interpolate (0, 0, 0) (255, 0, 0) ratio
      done;
      for i = (int_of_float x1) to (int_of_float x2) - 1 do
        let ratio = (float i -. x1) /. (x2 -. x1) in
        palette.(i) <- interpolate (255, 0, 0) (0, 255, 0) ratio
      done;
      for i = (int_of_float x2) to 255 do
        let ratio = (float i -. x2) /. (256.0 -. x2) in
        palette.(i) <- interpolate (0, 255, 255) (0, 0, 255) ratio
      done;
      palette

    let min : float -> float -> float =
      fun x y ->
      if x < y then x else y

    let max : float -> float -> float =
      fun x y ->
      if x > y then x else y
        
    let float_to_color minv maxv f =
      if f > 0.0 then
        let f = min maxv f in
        let f = max minv f in
        let f = (f -. minv) /. (maxv -. minv) in
        let r = Int32.of_float (f *. 255.0) in
        rgb r 0l 0l
      else
        let f = -. f in
        let f = min maxv f in
        let f = max minv f in
        let f = (f -. minv) /. (maxv -. minv) in
        let b = Int32.of_float (f *. 255.0) in
        rgb 0l 0l b

    open Bigarray
             
    type 'a data = (float, 'a, c_layout) Array2.t

    type 'a ticks = (float, 'a, c_layout) Array1.t

    let heatmap ~frame_clr ~minv ~maxv ~xticks ~yticks ~xdomain ~ydomain ~data =
      let xlength    = Array1.dim xdomain in
      let ylength    = Array1.dim ydomain in
      let xdata      = Array2.dim1 data in
      let ydata      = Array2.dim2 data in
      if xlength <> xdata || ylength <> ydata then
        failwith (Sdl.log "invalid heatmap params: |xdomain| = %d, |ydomain| = %d, |data| = %dx%d" xlength ylength xdata ydata; exit 1)
      else ();
      (* get min/max value in data *)
      (* let minv = ref max_float in *)
      (* let maxv = ref (-. max_float) in *)
      (* for i = 0 to xlength - 1 do *)
      (*   for j = 0 to ylength - 1 do *)
      (*     let f = data.{i, j} in *)
      (*     minv := min !minv f; *)
      (*     maxv := max !maxv f *)
      (*   done *)
      (* done; *)
      (* let minv       = !minv in *)
      (* let maxv       = !maxv in *)
      (* let idelta     = 1.0 /. (maxv -. minv) in *)
      let xrange     = { low = xdomain.{0}; high = xdomain.{xlength - 1} } in
      let yrange     = { low = ydomain.{0}; high = ydomain.{ylength - 1} } in
      let plot_image = Image.create xlength ylength in
      for i = 0 to xlength - 1 do
        for j = 0 to ylength - 1 do
          let f = data.{i, j} in
          let c = float_to_color minv maxv f in
          Image.set plot_image i j c
        done
      done;
      let image      = Commands.image ~pos:Pt.zero ~image:plot_image in
      axis
        ~xrange ~yrange
        ~xticks ~yticks
        ~xlength:(float xlength) ~ylength:(float ylength)
        ~clr:frame_clr
        ~data:[image]
        
  end
    
let init ~window_width ~window_height =
  (* Init SDL video *) 
  match Sdl.init Sdl.Init.video with
  | Error (`Msg msg) -> Sdl.log "Init error: %s" msg; exit 1
  | Ok () ->
     let window_result =
       Sdl.create_window
         ~w:window_width
         ~h:window_height
         "SDL OpenGL"
         Sdl.Window.opengl
     in
     match window_result with
     | Error (`Msg msg) -> Sdl.log "Create window error: %s" msg; exit 1
     | Ok window ->
        Sdl { window }

let render_layout_centered ctx xmargin ymargin layout =
  let (cmds, bbox) = Commands.emit_commands_with_bbox layout in
  let bbox = Commands.Bbox.of_commands cmds in
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
    
let display ~window ~plots =
  let Sdl { window } = window in
  let window_surface =
    match Sdl.get_window_surface window with
    | Error (`Msg msg) -> Sdl.log "Could not get window surface: %s" msg; exit 1    
    | Ok surface -> surface
  in
  let (screen_xsize, screen_ysize) = Sdl.get_window_size window in
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
    Bigarray.reshape_2 genarray screen_xsize screen_ysize
  in
  (* 4. Create a Cairo surface to write on the pixels *)
  let cairo_surface =
    Cairo.Image.create_for_data32 ~width:screen_xsize ~height:screen_ysize sdl_pixels
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
                x0 = 0.0 ; y0 = (float screen_ysize)
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
    render_layout_centered ctx 1.0 1.0 plots;
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

(* let window = init ~window_width:1080 ~window_height:1080 *)

(* let _ = display window layout *)
                  
(* let _ = *)
(*   Sdl.delay 2000l *)
               

    
(* let cairo_pdf_init width height outfile = *)
(*   let srf  = *)
(*     Cairo.PDF.create *)
(*       ~fname:outfile *)
(*       ~width:(float width) *)
(*       ~height:(float height) *)
(*   in *)
(*   let ctx  = Cairo.create srf  in *)
(*   let _    = *)
(*     Cairo.set_matrix *)
(*       ctx *)
(*       Cairo.({ *)
(*                 xx = 1.0 ; yx = 0.0 ; *)
(*                 xy = 0.0 ; yy = ~-. 1.0 ; *)
(*                 x0 = 0.0 ; y0 = (float height) *)
(*               }) *)
(*   in *)
(*   let _   = Cairo.set_line_width ctx 1.0 in *)
(*   let _   = Cairo.select_font_face ctx "DejaVuSansMono" ~slant:Cairo.Upright ~weight:Cairo.Normal in *)
(*   (ctx, srf) *)

(* let render_layout_centered ctx xmargin ymargin layout = *)
(*   let (cmds, bbox) = Commands.emit_commands_with_bbox layout in *)
(*   let bbox = Commands.Bbox.of_commands cmds in *)
(*   let w    = xmargin +. (Bbox.width bbox) *)
(*   and h    = ymargin +. (Bbox.height bbox) in *)
(*   let cmds = Commands.center_to_page (w, h) cmds in *)
(*   let _    = *)
(*     Cairo.set_matrix *)
(*       ctx *)
(*       Cairo.({ *)
(*                 xx = 1.0 ; yx = 0.0 ; *)
(*                 xy = 0.0 ; yy = ~-. 1.0 ; *)
(*                 x0 = 0.0 ; y0 = h *)
(*               }) *)
(*   in *)
(*   List.iter (Backend.render ctx) cmds; *)
(*   Cairo.show_page ctx   *)


(* let (context, surface) = cairo_init 500 500 "out.pdf" *)

(* let () = *)
(*   render_layout_centered surface context 1.0 1.0 layout; *)
(*   Cairo.Surface.finish surface *)
