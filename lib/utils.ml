open Bigarray
open Batteries

module Log = Log.Make(struct let section = "utils" end)
                     
type float_ref = { mutable x : float }
                   
let min (f : float) (f' : float) =
  if f < f' then f
  else f'

let max (f : float) (f' : float) =
  if f < f' then f'
  else f

let vector_range (data : Plot.data2d) =
  let len   = Owl.Mat.col_num data in
  let minv  = { x = max_float } in
  let maxv  = { x = (-. max_float) } in
  for i = 0 to len -1 do
    let v = Owl.Mat.get data 0 i in
    minv.x <- min minv.x v;
    maxv.x <- max maxv.x v
  done;
  (minv.x, maxv.x)
    
let data_range (data : Plot.data2d) =
  let xdata = Owl.Mat.row_num data in
  let ydata = Owl.Mat.col_num data in
  let minv  = { x = max_float } in
  let maxv  = { x = (-. max_float) } in
  for i = 0 to xdata -1 do
    for j = 0 to ydata -1 do
      let v = Owl.Mat.get data i j in
      minv.x <- min minv.x v;
      maxv.x <- max maxv.x v
    done
  done;
  (minv.x, maxv.x)

let interpolate a b n =
  if n < 2 then (Log.error "interpolate: not enough interpolation steps"; exit 1);
  let d = (b -. a) /. (float (n-1)) in
  let rec loop x i =
    if i = (n-1) then
      [b]
    else
      x :: (loop (x +. d) (i+1))
  in
  loop a 0

let rec _all_elements_equal x tl =
  match tl with
  | [] -> true
  | y :: tl ->
     (y = x) && (_all_elements_equal x tl)

let all_elements_equal l =
  match l with
  | [] | [_] -> true
  | x :: tl ->
     _all_elements_equal x tl
       
let (++) f g x = g (f x)
