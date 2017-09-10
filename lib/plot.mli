module Name :
  sig
    type t = int

    val compare : t -> t -> int

    val print : t -> string
  end


module Commands : Vlayout.Commands.CommandsSig

(** In an effort to make vplot compatible with owl, our vectors are really row vectors, i.e.
    matrices with one line. *)                    

type vector = Owl.Vec.vec
type data2d = Owl.Mat.mat

type target

type window

type plot = Commands.layout

val init_sdl : unit -> target

val init_pdf : string -> target

val display : target:target -> plot:plot -> unit
