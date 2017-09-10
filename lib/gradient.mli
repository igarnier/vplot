type path = (Vlayout.Style.color * float) list
type t = { rarr : float array; garr : float array; barr : float array; }
val begins_at_zero : float list -> bool
val ends_at_one : float list -> bool
val list_increasing : float list -> bool
val path_correct : ('a * float) list -> bool
val pair_list_elts : 'a list -> ('a * 'a) list
val interpolate : int -> (float * float) * (float * float) -> float array
val split_slices_by_colors :
  ((Vlayout.Style.color * 'a) * (Vlayout.Style.color * 'b)) list ->
  ((float * 'a) * (float * 'b)) list * ((float * 'a) * (float * 'b)) list *
  ((float * 'a) * (float * 'b)) list
val last_stop : ('a * 'b) list -> 'a
val create : (Vlayout.Style.color * float) list -> int -> t
