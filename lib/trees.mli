type 'a t =
  [ `Node of 'a * 'a t list ]

type trees_options =
  [ `Deltax of Units.mm
  | `Deltay of Units.mm
  | `Scalex of float
  | `Scaley of float
  | `Halign of Cmds.hposition
  | `Valign of Cmds.vposition
  ]

type options = trees_options

type 'a tree_type =
  | Simple     of { lbl : 'a -> Cmds.t list }
  | Dendrogram of { lbl : 'a -> Cmds.t list * float }

type 'a data =
  { tree : 'a t; tree_type : 'a tree_type }

val plot :
  options:options list ->
  viewport:Viewport.t ->
  data:'a data ->
  Cmds.t
  
