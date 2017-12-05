type 'a t =
  [ `Node of 'a * 'a t list ]

type 'a id = { elt : 'a; id : int }

type trees_options =
  [ `Deltax of Units.mm
  | `Deltay of Units.mm
  | `Scalex of float
  | `Scaley of float
  | `Halign  of Cmds.hposition
  | `Valign  of Cmds.vposition
  ]

type options_rec =
  {
    deltax : float;
    deltay : float;
    scalex : float;
    scaley : float;
    halign : Cmds.hposition;
    valign : Cmds.vposition
  }

type options = trees_options

type 'a tree_type =
  | Simple     of { lbl : 'a -> Cmds.t list }
  | Dendrogram of { lbl : 'a -> Cmds.t list * float }

type 'a data =
  { tree : 'a t; tree_type : 'a tree_type }

let default_options =
  {
    deltax = 10.;
    deltay = 50.;
    scalex = 1.0;
    scaley = 1.0;
    halign = `Hcentered;
    valign = `Vcentered
  }


let parse_options (options : options list) =
  List.fold_left (fun options_rec opt ->
      match opt with
      | `Deltax (deltax : Units.mm) -> { options_rec with deltax = (deltax :> float) }
      | `Deltay (deltay : Units.mm) -> { options_rec with deltay = (deltay :> float) }
      | `Scalex scalex -> { options_rec with scalex }
      | `Scaley scaley -> { options_rec with scaley }
      | `Halign halign   -> { options_rec with halign }
      | `Valign valign   -> { options_rec with valign }
    ) default_options options

let add_uids (tree : 'a t) =
  let uid =
    let x = ref 0 in
    fun elt -> let id = !x in incr x; { elt; id }
  in
  let rec traverse =
    function `Node(lbl, subtrees) ->
      `Node(uid lbl, List.map traverse subtrees)
  in
  traverse tree

let north cmds =
  let box = Cmds.Bbox.of_commands cmds in
  Cmds.Bbox.n box

let south cmds =
  let box = Cmds.Bbox.of_commands cmds in
  Cmds.Bbox.s box

let rec plot_tree params lblfunc (tree : 'a id t) =
  match tree with
  | `Node({ elt; id }, []) ->
    let cmds = lblfunc elt in
    let decl = Cmds.declpt ~pt:(north cmds) ~name:id in
    Cmds.cmd ~name:None (decl :: cmds)
  | `Node({ elt; id }, subtrees) ->
    let cmds     = lblfunc elt in
    let params   = { params with deltax = params.deltax *. params.scalex; deltay = params.deltay *. params.scaley } in
    let subplots = Cmds.hbox ~pos:params.halign ~deltax:params.deltax (List.map (plot_tree params lblfunc) subtrees) in
    let subnames = List.map (function `Node({ id }, _) -> id) subtrees in
    let ndecl    = Cmds.declpt ~pt:(north cmds) ~name:id in
    let sdecl    = Cmds.declpt ~pt:(south cmds) ~name:(-id) in
    let nodes =
      Cmds.vbox
        ~pos:params.valign
        ~deltay:params.deltay
        [Cmds.cmd ~name:None (sdecl::ndecl::cmds); subplots]
    in
    List.fold_left (fun layout id' -> 
        Cmds.arrow ~start:(-id) ~finish:id' ~sty:Cmds.Arrow.default_style layout
      ) nodes subnames

let rec plot_dendrogram params lblfunc (tree : 'a id t) =
  match tree with
  | `Node({ elt; id }, []) ->
    let cmds, _ = lblfunc elt in
    let decl    = Cmds.declpt ~pt:(north cmds) ~name:id in
    let cmd     = Cmds.cmd ~name:None (decl :: cmds) in
    cmd
  | `Node({ elt; id }, subtrees) ->
    let cmds, h  = lblfunc elt in
    let params   = { params with deltax = params.deltax *. params.scalex; deltay = params.deltay *. params.scaley } in
    let subplots = 
      List.map (plot_dendrogram params lblfunc) subtrees 
    in
    let subplots = Cmds.hbox ~pos:`Bottom ~deltax:params.deltax subplots in
    let subnames = List.map (function `Node({ id }, _) -> id) subtrees in
    let ndecl    = Cmds.declpt ~pt:(north cmds) ~name:id in
    let sdecl    = Cmds.declpt ~pt:(south cmds) ~name:(-id) in
    let nodes =
      Cmds.vbox
        ~pos:params.valign
        ~deltay:h
        [Cmds.cmd ~name:None (sdecl::ndecl::cmds); subplots]
    in
    List.fold_left (fun layout id' ->
        Cmds.arrow ~start:(-id) ~finish:id' ~sty:Cmds.Arrow.default_style layout
      ) nodes subnames
  
      
let plot ~options ~viewport ~data =
  let params = parse_options options in
  let layout =
    match data.tree_type with
    | Simple { lbl } ->
      plot_tree params lbl (add_uids data.tree)
    | Dendrogram { lbl } ->
      plot_dendrogram params lbl (add_uids data.tree)
  in
  let cmds   = Cmds.emit_commands layout in
  Viewport.apply viewport cmds
