open Numerics.Float64
open Vlayout (* for Pt *)

type vec =
  | Full of { data : Vec.t; sty : Style.t; lab : string }
  | Simple of Vec.t

type state = {
  mutable min_value : float;
  mutable max_value : float
}

type vector_options =
  [ `State of state ]

type vector_options_rec =
  {
    state : state
  }

type options =
  [ vector_options
  | Frame.options
  ]

type data = { domain : Vec.t; vecs : vec list }

let default_state () =
  {
    min_value  = max_float;
    max_value  = ~-. max_float;
  }

let default_options () =
  {
    state = default_state ()
  }

let set_option _vector_opt (opt : vector_options) =
  match opt with
  | `State state -> { state }

let parse_options (frame_opt, vector_opt) (option_list : options list) =
  List.fold_left
    (fun (frame_opt, vector_opt) opt ->
       match opt with
       | #Frame.options as o ->
         (Frame.set_option frame_opt o, vector_opt)
       | #vector_options as o ->
         (frame_opt, set_option vector_opt o)
    ) (frame_opt, vector_opt) option_list


let get_data = function
  | Full { data ; _ } | Simple data -> data

let _get_label = function
  | Full { lab ; _ } -> lab
  | Simple _     -> ""

let _map_data f =
  function
  | Full { data; sty; lab } ->
    Full { data = f data; sty; lab }
  | Simple data -> Simple (f data)

let _extract_ticks vec ticks =
  let len = Vec.length vec in
  if ticks < 2
  then invalid_arg "extract_ticks: not enough ticks on axis.";
  if len < ticks
  then invalid_arg "extract_ticks: too many ticks on axis.";
  let stride = len / (ticks - 1) in
  let acc    = ref [Vec.get vec 0] in
  for i = 1 to ticks - 2 do
    acc := (Vec.get vec (i * stride)) :: !acc
  done;
  List.rev ((Vec.get vec (len-1)) :: !acc)

(* Samples should be selected preferentially where the function has a lot
   of curvature, which for a triple of consecutive points corresponds to how
   "unflat" is the angle that they form. I.e. if we have data points y_1, y_2, y_3
   (and we assume the x_i are equispaced) then we get rid of y_2
   if (y_1 - y_2) dot (y_3 - y_2) < threshold (TODO: check) *)
let subsample data =
  let len = Vec.length data in
  let rec loop vprev vnow inow vnext acc =
    if inow = (len-2) then (inow + 1) :: inow :: acc
    else
      let _dot  = (vprev -. vnow) *. (vnext -. vnow) in (* TODO: normalise length of these vectors*)
      let iprev = inow
      and vprev = vnow
      and vnow  = vnext
      and inow  = inow + 1
      and vnext = Vec.get data (inow + 2) in
      if false (*TODO: insert appropriate test against well-chosen bound*) then
        (* Curvature et rid of vnow *)
        loop vprev vnow inow vnext acc
      else
        loop vprev vnow inow vnext (iprev :: acc)
  in
  if len = 0 then []
  else if len = 1 then [0]
  else if len = 2 then [0;1]
  else
    let vprev = Vec.get data 0
    and vnow  = Vec.get data 1
    and inow  = 1
    and vnext = Vec.get data 2
    and acc   = []
    in
    loop vprev vnow inow vnext acc

let vectors_enveloppe (data:Vec.t list) min_value max_value =
  List.fold_left (fun (min_value, max_value) data ->
      let (minv, maxv) = Utils.vector_range data in
      let min = Utils.min min_value minv
      and max = Utils.max max_value maxv in
      (min, max)
    ) (min_value, max_value) data

let draw_curve domain vec =
  let vector, sty =
    match vec with
    | Full { data ; sty ; _ } -> data, sty
    | Simple data ->
      let open Vlayout.Style in
      let sty = make ~stroke:(solid_stroke ~clr:red) ~width:None  ~dash:None~fill:None in
      data, sty
  in
  let samples = subsample vector in
  (* let samples = List.range 0 `To (Owl.Vec.numel vector - 1) in *)
  match List.rev samples with
  | [] | [_] ->
    failwith "draw_curve: sample list too short. Bug found."
  | index :: tl ->
    let y      = Vec.get vector index in
    let x      = Vec.get domain index in
    let pt     = Pt.pt x y in
    let segments, _ =
      List.fold_left (fun (segments, prevpt) index ->
          let y   = Vec.get vector index in
          let x   = Vec.get domain index in
          let pt  = Pt.pt x y in
          let seg = Cmds.segment ~p1:prevpt ~p2:pt in
          (seg :: segments, pt)
        ) ([], pt) tl
    in
    Cmds.([style ~style:sty ~subcommands:segments])

let plot_internal frame { state } viewport domain data =
  begin match data with [] -> invalid_arg "plot_internal: empty data list on input" | _ -> () end;
  let points = List.map get_data data in
  let datlen = List.map Vec.length points in
  if not (Utils.all_elements_equal datlen) then
    invalid_arg "plot_internal: input vectors must all be of identical length"
  else ();
  let datlen = List.hd datlen in
  let domlen = Vec.length domain in
  if domlen <> datlen then
    invalid_arg (Printf.sprintf "plot_internal: size mismatch: |domain| = %d, |data| = %d" domlen datlen)
  else ();
  let minv, maxv =
    let minv, maxv = vectors_enveloppe points state.min_value state.max_value in
    state.min_value <- minv;
    state.max_value <- maxv;
    minv, maxv
  in
  let minv, maxv =
    (* Handle near-constant functions *)
    if abs_float (minv -. maxv) < (epsilon_float *. 100.) then
      ((minv -. 1.0), (maxv +. 1.0))
    else
      (minv, maxv)
  in
  (* Compute plot *)
  let curves =
    List.map (draw_curve domain) data
    |> List.flatten
  in
  let ydomain = Utils.linspace minv maxv frame.Frame.yaxis.tick_num in
  (* Add frame *)
  let _, framed = Frame.add_frame frame (Vec.to_array domain) ydomain curves in
  (* Map to viewport *)
  Viewport.apply viewport [framed]

let plot ~options ~viewport ~data =
  let { domain; vecs } = data in
  let frame, hmap_opts = parse_options (Frame.default, default_options ()) options in
  plot_internal frame hmap_opts viewport domain vecs
