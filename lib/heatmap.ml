open Vlayout
open Bigarray
       
module Log = Log.Make(struct let section = "Heatmap" end)
                     
type gradient_spec =
  { gradient_path : Gradient.path; gradient_steps : int }

(* Some of the parameters are meant to be only set once, at plot creation time.
   Those must be passed as arguments during class creation. We use optional
   arguments to set default values. *)

let default_blocksize = (1,1)

let default_gradient =
  let gradient_path = 
    [ (Style.black, 0.0); (Style.gray 0.5, 0.5); (Style.red, 1.0) ]
  in
  let gradient_steps = 50 in
  { gradient_path; gradient_steps }

(* [axis] implements a standard x/y axis *)
type range = { low : float; high : float }

let clamp (f : float) =
  if f < 0.0 then 0.0
  else if f > 1.0 then 1.0
  else f

let write_data_to_image_noblock palette width minv maxv xlength ylength data =
  let plot_image = Image.create xlength ylength in
  let ilen       = 1.0 /. (maxv -. minv) in
  for i = 0 to xlength - 1 do
    for j = 0 to ylength - 1 do
      let f = Owl.Mat.get data i j in
      let f = clamp ((f -. minv) *. ilen) in
      let c = int_of_float (width *. f) in
      Image.set plot_image i j palette.{c}
    done
  done;
  plot_image
    
let write_data_to_image block_x block_y palette width minv maxv xlength ylength data =
  let ximage     = xlength * block_x
  and yimage     = ylength * block_y in
  let plot_image = Image.create ximage yimage in
  let ilen       = 1.0 /. (maxv -. minv) in
  for i = 0 to xlength - 1 do
    for j = 0 to ylength - 1 do
      let f = Owl.Mat.get data i j in
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

module Commands = Plot.Commands                    

let heatbar minv maxv yticks tick_length ylabel_to_tick orig xlength ylength heatbar_xoffset clr gradient_path =
  let heatbar_xoffset = 50.0 in
  let heatbar_width   = 30.0 in
  let heatbar_xpos    = orig.Pt.x +. xlength +. heatbar_xoffset in
  let heatbar_style   = Style.(make
                                 ~stroke:(solid_stroke ~clr)
                                 ~fill:(Some (vertical_gradient gradient_path))
                              )
  in
  let mins      = Pt.pt heatbar_xpos orig.y in
  let maxs_x    = heatbar_xpos +. heatbar_width in
  let maxs_y    = orig.y +. ylength in
  let maxs      = Pt.pt maxs_x maxs_y in
  let bar       = Commands.style ~style:heatbar_style ~subcommands:[ Commands.box mins maxs ] in
  let ticks_y   = Utils.interpolate minv maxv yticks in
  let pos_y     = Utils.interpolate 0.0 ylength yticks in
  let bar_ticks = List.fold_left2 (fun acc ypos ylab ->
      let tick = Commands.segment
          ~p1:(Pt.pt maxs_x ypos)
          ~p2:(Pt.pt (maxs_x -. tick_length) ypos) in
      let labl = Commands.text
          ~pos:({ pos = Pt.pt (maxs_x -. tick_length +. ylabel_to_tick) ypos; relpos = West })
          ~size:10.0
          ~text:(Common.float_to_string ylab)
      in
      tick :: labl :: acc
    ) [] pos_y ticks_y
  in
  let bar_ticks =
    Commands.style
      ~style:Style.(make ~stroke:(solid_stroke ~clr) ~fill:None)
      ~subcommands:bar_ticks
  in
  [bar; bar_ticks]


class t ?(gradient=default_gradient) ?(blocksize=default_blocksize) () =
  let { gradient_path; gradient_steps } = default_gradient in
  let { Gradient.rarr; garr; barr } =
    Gradient.create gradient_path gradient_steps
  in
  let palette, width =
    let len = Array.length rarr in
    let arr = Array1.create int32 c_layout len in
    for i = 0 to len - 1 do
      arr.{i} <- Style.(to_int (rgb rarr.(i) garr.(i) barr.(i)))
    done;
    arr, (float (len - 1))
  in    

  object (self)
    (* Default values for the plot parameters *)
    val mutable xlabel_to_tick   = ~-. 10.
    val mutable ylabel_to_tick   = ~-. 10.
    val mutable tick_length      = 10.
    val mutable xticks           = 6
    val mutable yticks           = 6
    val mutable text_size        = 10.
    val mutable min_cutoff       = ~-. max_float
    val mutable max_cutoff       = max_float
    val mutable blocksize        = blocksize
    val mutable frame_color      = Style.gray 0.5
    val mutable background_color = Style.black
    val mutable caption          = ""
    val mutable dynamic          = false
    (* Internal state, used for dynamic plots *)
    val mutable min_value        = max_float
    val mutable max_value        = ~-. max_float
    (* boilerplate *)
    method xlabel_to_tick = xlabel_to_tick
    method set_xlabel_to_tick f = xlabel_to_tick <- f

    method ylabel_to_tick = ylabel_to_tick
    method set_ylabel_to_tick f = ylabel_to_tick <- f

    method tick_length = tick_length
    method set_tick_length f = tick_length <- f

    method xticks = xticks
    method set_xticks i = xticks <- i

    method yticks = yticks
    method set_yticks i = yticks <- i
                            
    method text_size = text_size
    method set_text_size f = text_size <- f

    method min_cutoff = min_cutoff
    method set_min_cutoff f = min_cutoff <- f

    method max_cutoff = max_cutoff
    method set_max_cutoff f = max_cutoff <- f
                                
    method frame_color = frame_color
    method set_frame_color c = frame_color <- c
                                 
    method background_color = background_color
    method set_background_color c = background_color <- c

    method caption = caption
    method set_caption s = caption <- s

    method dynamic = dynamic
    method set_dynamic b = dynamic <- b

    method plot ~(xdomain : Plot.vector)  ~(ydomain : Plot.vector) ~(data : Plot.data2d) =
      let xlength    = Owl.Vec.numel xdomain in  
      let ylength    = Owl.Vec.numel ydomain in
      let xdata      = Owl.Mat.row_num data in
      let ydata      = Owl.Mat.col_num data in
      if xlength <> xdata || ylength <> ydata then
        (Log.error "invalid heatmap size: |xdomain| = %d, |ydomain| = %d, |data| = %dx%d" xlength ylength xdata ydata; exit 1)
      else ();
      let xrange     = { low = Owl.Vec.get xdomain 0; high = Owl.Vec.get xdomain (xlength - 1) } in
      let yrange     = { low = Owl.Vec.get ydomain 0; high = Owl.Vec.get ydomain (ylength - 1) } in
      let minv, maxv =
        if self#dynamic then
          let minv, maxv = Utils.data_range data in
          let minv       = Utils.min min_value minv
          and maxv       = Utils.max max_value maxv in
          min_value <- minv;
          max_value <- maxv;
          minv, maxv
        else
          self#min_cutoff, self#max_cutoff
      in
      let plot_image =
        match blocksize with
        | (1,1) -> write_data_to_image_noblock palette width minv maxv xlength ylength data
        | (i,j) -> write_data_to_image i j palette width minv maxv xlength ylength data
      in
      let image      = Commands.image ~pos:(Pt.pt 1.0 1.0) ~image:plot_image in
      let xlength    = Image.xsize plot_image
      and ylength    = Image.ysize plot_image in

      let ticks_x    = Utils.interpolate xrange.low xrange.high self#xticks in
      let ticks_y    = Utils.interpolate yrange.low yrange.high self#yticks in
      let vertical_axis   =
        Common.ticked_vertical_axis
          ~tick_text_size:self#text_size
          ~label_to_tick:self#ylabel_to_tick
          ~tick_length:self#tick_length
          ~origin:Pt.zero
          ~length:(float ylength)
          ~tick_labels:ticks_y
      in
      let horizontal_axis =
        Common.ticked_horizontal_axis
          ~tick_text_size:self#text_size
          ~label_to_tick:self#xlabel_to_tick
          ~tick_length:self#tick_length
          ~origin:Pt.zero
          ~length:(float xlength)
          ~tick_labels:ticks_x
      in
      let heatbar =
        let heatbar_xoffset = 30. in
        heatbar
          min_value max_value
          yticks (~-. tick_length) (~-. ylabel_to_tick)
          Pt.zero
          (float xlength) (float ylength)
          heatbar_xoffset
          frame_color
          gradient_path
      in
      Common.(
        apply_frame_color self#frame_color (image :: (vertical_axis @ horizontal_axis @ heatbar))
        |> command
        |> apply_label_below self#caption self#text_size self#frame_color 30. 
      )

  end
