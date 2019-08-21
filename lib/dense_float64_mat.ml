open Bigarray

type elt = float
type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Array2.t


let create ~(lines : int) ~(cols : int) =
  let mat = Array2.create Bigarray.Float64 Bigarray.C_layout lines cols in
  Array2.fill mat 0.0 ;
  mat

let init ~(lines : int) ~(cols : int) ~f =
  let mat = Array2.create Bigarray.Float64 Bigarray.C_layout lines cols in
  for i = 0 to lines - 1 do
    for j = 0 to cols - 1 do
      Array2.set mat i j (f i j)
    done
  done ;
  mat

let dim1 (mat : t) =
  Array2.dim1 mat

let dim2 (mat : t) =
  Array2.dim2 mat

let shape (mat : t) = (dim1 mat, dim2 mat)

let set (mat : t) i j v =
  Array2.set mat i j v

let get (mat : t) i j =
  Array2.get mat i j

let unsafe_set (mat : t) i j v =
  Array2.unsafe_set mat i j v

let unsafe_get (mat : t) i j =
  Array2.unsafe_get mat i j

let map (mat : t) (f : float -> float) =
  let res = create ~lines:(dim1 mat) ~cols:(dim2 mat) in
  for i = 0 to dim1 mat - 1 do
    for j = 0 to dim2 mat - 1 do
      set res i j (f (get mat i j))
    done
  done ;
  res

let column (mat : t) (i : int) =
  let lines, cols = shape mat in
  if i >= cols then
    let msg =
      Printf.sprintf
        "Matrix.column: matrix has shape %d x %d, column %d out of bounds"
        lines cols i
    in
    Pervasives.failwith msg
  else
    let col = create ~lines ~cols:1 in
    for k = 0 to lines - 1 do
      set col k 0 (get mat k i)
    done ;
    col

let is_column (m : t) : bool =
  let _, cols = shape m in
  cols = 1

let all_equal (l : int list) : int option =
  let result =
    List.sort_uniq (fun (x : int) (y : int) ->
        Pervasives.compare x y
      ) l in
  match result with
  | [ uniq ] -> Some uniq
  | _        -> None

let concat_columns_horiz (columns : t list) : t =
  let cols = List.length columns in
  if cols = 0 then
    Pervasives.failwith "concat_columns_horiz: empty list of columns" ;
  if not (List.for_all is_column columns) then
    Pervasives.failwith "concat_columns_horiz: invalid argument" ;
  let row_dims = List.map dim1 columns in
  match all_equal row_dims with
  | None ->
    Pervasives.failwith "concat_columns_horiz: invalid argument"
  | Some rows ->
    let columns = Array.of_list columns in
    init ~lines:rows ~cols ~f:(fun l c ->
        get columns.(c) l 0
      )

let binop op mat1 mat2 =
  let f1 = Array2.change_layout mat1 Fortran_layout in
  let f2 = Array2.change_layout mat2 Fortran_layout in
  let fr = op f1 f2 in
  Array2.change_layout fr C_layout
[@@inline]

let copy mat =
  let lines, cols = shape mat in
  let mat' = create ~lines ~cols in
  Array2.blit mat mat' ;
  mat'
