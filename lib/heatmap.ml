open Vlayout
       
module Log = Log.Make(struct let section = "Heatmap" end)
                               
module type Params =
  sig

    val gradient       : Gradient.path
    val gradient_steps : int
    val frame_color    : Style.color
    val blocksize      : (int * int) option
    val tick_length    : float option
    val xlabel_to_tick : float option
    val ylabel_to_tick : float option
                               
  end


module Make(P : Params) =
  struct

    (* Check that parameters make sense *)
    let _ =
      if P.gradient_steps <= (List.length P.gradient) then
        (Log.error "gradient_steps must be > (List.length gradient)";
         exit 1)
      else ()

    let _ =
      if not (Gradient.path_correct P.gradient) then
        (Log.error "incorrect gradient path";
         exit 1)
      else ()

    let _ =
      match P.blocksize with
      | None -> ()
      | Some (block_x, block_y) when (block_x <= 0 || block_y <= 0) ->
         (Log.error "incorrect block size: block size must be strictly positive";
          exit 1)
      | _ -> ()
             
    (* Set default values for the optional parameters if needed *)             
    let tick_length =
      match P.tick_length with
      | None      -> ~-. 10.0
      | Some len  -> ~-. (abs_float len)

    let xlabel_to_tick =
      match P.xlabel_to_tick with
      | None      -> 10.0
      | Some len  -> abs_float len

    let ylabel_to_tick =
      match P.ylabel_to_tick with
      | None      -> 10.0
      | Some len  -> abs_float len

    let block_x, block_y =
      match P.blocksize with
      | None                    -> (1, 1)
      | Some (block_x, block_y) -> (block_x, block_y)

    (* [axis] implements a standard x/y axis *)
    type range = { low : float; high : float }
                   
    let interpolate a b n =
      let d = (b -. a) /. (float n) in
      let rec loop x i =
        if i = n then
          [b]
        else
          x :: (loop (x +. d) (i+1))
      in
      loop a 0

    let float_to_string f = Printf.sprintf "%g" f

    module Commands = Plot.Commands
           
    let axis ~minv ~maxv ~xrange ~yrange ~xticks ~yticks ~xlength ~ylength ~clr ~data ~label =
      (* produce commands *)
      Commands.(
        let orig  = Pt.zero in
        let frame =
          let ticks_x    = interpolate xrange.low xrange.high xticks in
          let ticks_y    = interpolate yrange.low yrange.high yticks in
          let pos_x      = interpolate 0.0 xlength xticks in
          let pos_y      = interpolate 0.0 ylength yticks in
          let x_segment  = segment ~p1:orig ~p2:(Pt.pt (orig.x +. xlength) orig.y) in
          let y_segment  = segment ~p1:orig ~p2:(Pt.pt orig.x (orig.y +. ylength)) in
          let xtick_cmds = List.fold_left2 (fun acc xpos xlab ->
                               let tick = segment
                                            ~p1:(Pt.pt xpos orig.y)
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
                                            ~p1:(Pt.pt orig.x ypos)
                                            ~p2:(Pt.pt tick_length ypos) in
                               let labl = text
                                            ~pos:({ pos = Pt.pt (~-. ylabel_to_tick) ypos; relpos = East })
                                            ~size:10.0
                                            ~text:(float_to_string ylab)
                               in
                               tick :: labl :: acc
                             ) [] pos_y ticks_y
          in
          style
            ~style:Style.(make ~stroke:(solid_stroke ~clr) ~fill:None)
            ~subcommands:([ x_segment; y_segment ] @ xtick_cmds @ ytick_cmds)
        in
        let heatbar =
          let heatbar_xoffset = 50.0 in
          let heatbar_width   = 30.0 in
          let heatbar_xpos    = orig.x +. xlength +. heatbar_xoffset in
          let heatbar_style   = Style.(make
                                         ~stroke:(solid_stroke ~clr)
                                         ~fill:(Some (vertical_gradient P.gradient))
                                )
          in
          let mins      = Pt.pt heatbar_xpos orig.y in
          let maxs_x    = heatbar_xpos +. heatbar_width in
          let maxs_y    = orig.y +. ylength in
          let maxs      = Pt.pt maxs_x maxs_y in
          let bar       = style ~style:heatbar_style ~subcommands:[ box mins maxs ] in
          let ticks_y   = interpolate minv maxv yticks in
          let pos_y     = interpolate 0.0 ylength yticks in
          let bar_ticks = List.fold_left2 (fun acc ypos ylab ->
                             let tick = segment
                                          ~p1:(Pt.pt maxs_x ypos)
                                          ~p2:(Pt.pt (maxs_x -. tick_length) ypos) in
                             let labl = text
                                          ~pos:({ pos = Pt.pt (maxs_x -. tick_length +. ylabel_to_tick) ypos; relpos = West })
                                          ~size:10.0
                                          ~text:(float_to_string ylab)
                             in
                             tick :: labl :: acc
                           ) [] pos_y ticks_y
          in
          let bar_ticks =
            style
              ~style:Style.(make ~stroke:(solid_stroke ~clr) ~fill:None)
              ~subcommands:bar_ticks
          in
          [bar; bar_ticks]
        in
        let label =
          cmd ~name:None
              [ style
                  ~style:(Style.make
                            ~stroke:(Style.solid_stroke ~clr)
                            ~fill:None)
                  ~subcommands:
                  [
                    text
                      ~pos:{ pos = Pt.zero; relpos = South }
                      ~size:10.0
                      ~text:label
                  ]
              ]
        in
        vbox
          ~deltay:30.0
          ~layout_list:
          [ cmd ~name:None (frame :: (heatbar @ data));
            label
          ]
      )
             
    (* Prepare gradients according to P.gradient *)
    let { Gradient.rarr; garr; barr } =
      Gradient.create P.gradient P.gradient_steps

    open Bigarray
                      
    (* Convert colors to int32s for better writing in memory *)
    let palette, width =
      let len = Array.length rarr in
      let arr = Array1.create int32 c_layout len in
      for i = 0 to len - 1 do
        arr.{i} <- Style.(to_int (rgb rarr.(i) garr.(i) barr.(i)))
      done;
      arr, len

    let float_to_color width minv ilen f =
      let v = width *. (f -. minv) *. ilen in (* if f is correct, v \in [0,1] *)
      let i = int_of_float v in
      palette.{i}
             
    let range_delta { low; high } = high -. low

    let clamp (f : float) =
      if f < 0.0 then 0.0
      else if f > 1.0 then 1.0
      else f

    let write_data_to_image_noblock width minv maxv xlength ylength data =
      let plot_image = Image.create xlength ylength in
      let ilen       = 1.0 /. (maxv -. minv) in
      for i = 0 to xlength - 1 do
        for j = 0 to ylength - 1 do
          let f = data.{i, j} in
          let f = clamp ((f -. minv) *. ilen) in
          let c = int_of_float (width *. f) in
          Image.set plot_image i j palette.{c}
        done
      done;
      plot_image

    let write_data_to_image width minv maxv xlength ylength data =
      let ximage     = xlength * block_x
      and yimage     = ylength * block_y in
      let plot_image = Image.create ximage yimage in
      let ilen       = 1.0 /. (maxv -. minv) in
      for i = 0 to xlength - 1 do
        for j = 0 to ylength - 1 do
          let f = data.{i, j} in
          let f = clamp ((f -. minv) *. ilen) in
          let c = int_of_float (width *. f) in
          let c = palette.{c} in
          let index_i = i * block_x in
          let index_j = j * block_y in
          for block_i = 0 to block_x - 1 do
            for block_j = 0 to block_y - 1 do
              let i' = index_i + block_i
              and j' = index_j + block_j in
              Image.set plot_image i' j' c
            done
          done
        done
      done;
      plot_image

    type float_ref = { mutable x : float }
        
    let min (f : float) (f' : float) =
      if f < f' then f
      else f'

    let max (f : float) (f' : float) =
      if f < f' then f'
      else f

    let data_range (data : Plot.data2d) =
      let xdata = Array2.dim1 data in
      let ydata = Array2.dim2 data in
      let minv  = { x = max_float } in
      let maxv  = { x = (-. max_float) } in
      for i = 0 to xdata -1 do
        for j = 0 to ydata -1 do
          let v = data.{i, j} in
          minv.x <- min minv.x v;
          maxv.x <- max maxv.x v
        done
      done;
      (minv.x, maxv.x)

    (* Global refs to keep track of the range of the data across several frames. *)        
    let minvref = { x = max_float }
    let maxvref = { x = -. max_float }
        
    let heatmap
          width
          ~dynamic
          ~minv
          ~maxv
          ~xticks
          ~yticks
          ~(xdomain : Plot.domain)
          ~(ydomain : Plot.domain)
          ~(data : Plot.data2d)
          ~(label : string)
      =
      let xlength    = Array1.dim xdomain in
      let ylength    = Array1.dim ydomain in
      let xdata      = Array2.dim1 data in
      let ydata      = Array2.dim2 data in
      if xlength <> xdata || ylength <> ydata then
        (Log.error "invalid heatmap size: |xdomain| = %d, |ydomain| = %d, |data| = %dx%d" xlength ylength xdata ydata; exit 1)
      else ();
      let xrange     = { low = xdomain.{0}; high = xdomain.{xlength - 1} } in
      let yrange     = { low = ydomain.{0}; high = ydomain.{ylength - 1} } in
      let minv, maxv =
        if dynamic then
          let minv, maxv = data_range data in
          let minv       = min minvref.x minv
          and maxv       = max maxvref.x maxv in
          minvref.x <- minv;
          maxvref.x <- maxv;
          minv, maxv
        else
          minv, maxv
      in
      let plot_image =
        match P.blocksize with
        | None   -> write_data_to_image_noblock width minv maxv xlength ylength data
        | Some _ -> write_data_to_image width minv maxv xlength ylength data
      in
      let image      = Commands.image ~pos:(Pt.pt 1.0 1.0) ~image:plot_image in
      let xlength    = Image.xsize plot_image
      and ylength    = Image.ysize plot_image in
      axis
        ~minv ~maxv
        ~xrange ~yrange
        ~xticks ~yticks
        ~xlength:(float xlength) ~ylength:(float ylength)
        ~clr:P.frame_color
        ~data:[image]
        ~label

    let heatmap ?dynamic:(dyn=false) ~minv ~maxv ~xticks ~yticks ~xdomain ~ydomain ~data ~label () =
      heatmap ~dynamic:dyn ~minv ~maxv ~xticks ~yticks ~xdomain ~ydomain ~data ~label (float (width - 1))

  end
