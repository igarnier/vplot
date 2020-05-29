open Vlayout

(* Instantiate the Vlayout.Commands functor *)
module Name = struct
  type t = int

  let compare (x : int) (y : int) = if x > y then 1 else if x = y then 0 else -1

  let pp = Format.pp_print_int
end

include Commands.Make (Name)
