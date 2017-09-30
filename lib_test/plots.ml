open Vplot

let pi = acos (~-. 1.)

let domain = Owl.Vec.linspace 0.0 (2. *. pi) 300

let plot =
  let vplot = new Vector.t () in
  vplot#set_caption "test caption";
  vplot#set_frame_color (Vlayout.Style.gray 0.7);
  vplot#plot
    ~domain:domain
    ~data:[
      Simple (Owl.Vec.sin domain);
      Full { data = Owl.Vec.cos domain;
             clr  = Vlayout.Style.red;
             lab  = "cos" };
      Full { data = Owl.Vec.tanh domain;
             clr  = Vlayout.Style.blue;
             lab  = "cos" };
    ]


let target = Plot.init_pdf "plot.pdf"

let _ = Plot.display ~target ~plot
