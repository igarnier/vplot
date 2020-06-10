let colors =
  [ Vlayout.Color.red;
    Vlayout.Color.green;
    Vlayout.Color.blue;
    Vlayout.Color.gray 0.5;
    Vlayout.Color.black;
    Vlayout.Color.pink;
    Vlayout.Color.cyan ]

let solid_styles =
  [ Vlayout.Style.Solid.red;
    Vlayout.Style.Solid.green;
    Vlayout.Style.Solid.blue;
    Vlayout.Style.Solid.gray 0.5;
    Vlayout.Style.Solid.black;
    Vlayout.Style.Solid.pink;
    Vlayout.Style.Solid.cyan ]

let dot_styles =
  [ Vlayout.Style.DotDash.red;
    Vlayout.Style.DotDash.green;
    Vlayout.Style.DotDash.blue;
    Vlayout.Style.DotDash.gray 0.5;
    Vlayout.Style.DotDash.black;
    Vlayout.Style.DotDash.pink;
    Vlayout.Style.DotDash.cyan ]

let medium_styles =
  [ Vlayout.Style.MediumDash.red;
    Vlayout.Style.MediumDash.green;
    Vlayout.Style.MediumDash.blue;
    Vlayout.Style.MediumDash.gray 0.5;
    Vlayout.Style.MediumDash.black;
    Vlayout.Style.MediumDash.pink;
    Vlayout.Style.MediumDash.cyan ]

let large_styles =
  [ Vlayout.Style.LargeDash.red;
    Vlayout.Style.LargeDash.green;
    Vlayout.Style.LargeDash.blue;
    Vlayout.Style.LargeDash.gray 0.5;
    Vlayout.Style.LargeDash.black;
    Vlayout.Style.LargeDash.pink;
    Vlayout.Style.LargeDash.cyan ]

let all_styles = solid_styles @ dot_styles @ medium_styles @ large_styles

let styles_num = List.length all_styles

let rec take_n n l acc =
  match l with
  | [] -> if n > 0 then failwith "take_n" else List.rev acc
  | hd :: tl -> if n = 0 then List.rev acc else take_n (n - 1) tl (hd :: acc)

let take_n n l = take_n n l []

let plot ?name ?(options = []) domain vecs =
  let vecs_num = List.length vecs in
  if vecs_num > styles_num then
    invalid_arg "Easy.plot: too many vectors. Please use Plot module." ;
  let styles = take_n vecs_num all_styles in
  let vecs =
    List.map2 (fun data sty -> Vector.Full { data; sty; lab = "" }) vecs styles
  in
  let viewport = Viewport.AutoY { xsize = Units.mm 150.0 } in
  let result = Vector.plot ~options ~viewport ~data:{ domain; vecs } in
  let layout = Plot.Cmd [result] in
  match name with
  | None ->
      (* SDL plot *)
      let spec =
        Window.(
          Size_and_dpi
            { size = { width = Units.mm 1024.; height = Units.mm 1024. };
              dpi = 100.
            })
      in
      Plot.plot_sdl ~spec ~mode:Window.Preserve_aspect layout
  | Some name ->
      (* PDF plot *)
      Plot.plot_pdf name layout

let shapes =
  let open Scatter in
  let radius = 0.05 in
  let length = 2.0 *. radius in
  [ Dot { radius };
    Circle { radius };
    Square { length };
    Cross { length };
    Plus { length } ]

let rec cartesian_product l1 l2 acc =
  match l1 with
  | [] -> acc
  | hd :: tl -> cartesian_product tl l2 (List.map (fun y -> (hd, y)) l2 @ acc)

let cartesian_product l1 l2 = cartesian_product l1 l2 []

let shapes_and_colors = cartesian_product shapes colors

let assert_size xs ys =
  if Array.length xs <> Array.length ys then
    invalid_arg "Easy.scatter: xs/ys vectors of different sizes"

let scatter ?name ?(options = []) cloud_list =
  let cloud_list =
    List.map
      (fun (xs, ys) ->
        assert_size xs ys ;
        Array.map2 (fun x y -> Vlayout.Pt.pt x y) xs ys)
      cloud_list
  in
  let len = List.length cloud_list in
  if len > List.length shapes_and_colors then
    invalid_arg "Easy.scatter: too many clouds. Please use scatter module." ;
  let viewport = Viewport.AutoY { xsize = Units.mm 150.0 } in
  let data =
    List.combine cloud_list (take_n len shapes_and_colors)
    |> List.map (fun (data, (shape, color)) ->
           Scatter.{ data; plot_type = Scatter { shape; color } })
  in
  let result = Scatter.plot ~options ~viewport ~data in
  let layout = Plot.Cmd [result] in
  match name with
  | None ->
      (* SDL plot *)
      let spec =
        Window.(
          Size_and_dpi
            { size = { width = Units.mm 1024.; height = Units.mm 1024. };
              dpi = 100.
            })
      in
      Plot.plot_sdl ~spec ~mode:Window.Preserve_aspect layout
  | Some name ->
      (* PDF plot *)
      Plot.plot_pdf name layout
