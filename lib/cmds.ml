open Vlayout
    
(* Instantiate the Vlayout.Commands functor *)
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

include Commands
