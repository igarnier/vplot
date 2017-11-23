
type 'a t =
  {
    fold : 'b. leaf:('a -> 'b) -> node:('a -> 'b list -> 'b) -> 'b
  }
  
let plot ~lbl ~data =
  data.fold 
    ~leaf:lbl
    ~node:(fun label subtrees ->
        let subtrees = Cmds.hbox ~deltax:1.0 ~layout_list:subtrees in
        Cmds.vbox
          ~deltay:1.0
          ~layout_list:[
            lbl label;
            subtrees
          ]
      )

(* module type TreeSig = *)
(* sig *)

(*   type t *)

(*   type label *)

(*   val fold : *)
(*     t -> *)
(*     (label -> 'a) -> *)
(*     (label -> 'a list -> 'a) -> *)
(*     'a *)

(* end *)


(* let mktree label sublayouts = *)



(* module Make = *)
(*   functor (T : TreeSig) -> *)
(*   struct *)

(*     class t = *)
(*       object *)

(*         method dendrogram ~leafplot ~weight ~tree = *)
(*           T.fold tree *)
(*             leafplot *)
(*             (fun labels sublayouts -> *)

(*             ) *)

(*       end *)

(*   end *)
