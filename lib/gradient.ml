type path = (Vlayout.Color.t * float) list

type t = { rarr : float array; garr : float array; barr : float array }

let begins_at_zero ofs = match ofs with 0.0 :: _ -> true | _ -> false

let ends_at_one ofs = match List.rev ofs with 1.0 :: _ -> true | _ -> false

(* assumes (begins_at_zero ofs) = true *)
let list_increasing ofs =
  let (_, result) =
    List.fold_left
      (fun (prev, flag) elt -> (elt, prev < elt && flag))
      (0.0, true)
      (List.tl ofs)
  in
  result

let path_correct path =
  let ofs = List.map snd path in
  begins_at_zero ofs && ends_at_one ofs && list_increasing ofs

let pair_list_elts =
  let rec loop list prev =
    match list with
    | [] -> failwith "pair_list_elts: error"
    | [x] -> [(prev, x)]
    | x :: tl -> (prev, x) :: loop tl x
  in
  fun list ->
    match list with
    | [] -> failwith "pair_list_elts: error"
    | x :: tl -> loop tl x

let interpolate total_len ((v1, ofs1), (v2, ofs2)) =
  let deltaofs = ofs2 -. ofs1 in
  let len = int_of_float (float total_len *. deltaofs) in
  let arr = Array.make len 0.0 in
  let invl = 1.0 /. float len in
  let incr = (v2 -. v1) *. invl in
  for i = 0 to len - 1 do
    let v = v1 +. (incr *. float i) in
    arr.(i) <- v
  done ;
  arr

let split_slices_by_colors slices =
  let open Vlayout.Color in
  let mapclr proj slices =
    List.map
      (fun ((c1, ofs1), (c2, ofs2)) -> ((proj c1, ofs1), (proj c2, ofs2)))
      slices
  in
  ( mapclr (fun { r; _ } -> r) slices,
    mapclr (fun { g; _ } -> g) slices,
    mapclr (fun { b; _ } -> b) slices )

let last_stop l =
  let (clr, _) = List.hd (List.rev l) in
  clr

let create path total_len =
  if not (path_correct path) then failwith "Gradient.create: path is incorrect"
  else
    let slices = pair_list_elts path in
    let (rslices, gslices, bslices) = split_slices_by_colors slices in
    let rslices = List.map (interpolate total_len) rslices in
    let gslices = List.map (interpolate total_len) gslices in
    let bslices = List.map (interpolate total_len) bslices in
    let lastclr = last_stop path in
    let rarr = Array.(append (concat rslices) [| lastclr.Vlayout.Color.r |]) in
    let garr = Array.(append (concat gslices) [| lastclr.Vlayout.Color.g |]) in
    let barr = Array.(append (concat bslices) [| lastclr.Vlayout.Color.b |]) in
    { rarr; garr; barr }
