
(** type [t] is an abstract type that represents the gui window. [t] 
    contains information about the width and height of the window.
    [t] also contains information for the images that will be displayed
    in the window. *)
type t 

(** [make_gui] creates a graphical window based on the dimensions that are
    input. Opens graphical window and instantiates initial game canvas.
    Required: valid dimensions that can display on a user's screen. *)
val make_gui : int -> int -> t
(*
(** [draw_player] draws player onto gui *)
val draw_player: t -> unit

(** [draw_obstacles] draws obstacles onto gui *)
val draw_obstacles: t -> unit

(** [draw_score] draws score onto gui *)
val draw_score: t -> unit

(** [draw_floor] draws floor onto gui *)
val draw_floor: t -> unit 

(** [draw_gameover] draws gameover onto gui *)
val draw_gameover: t -> unit

(** [draw_pause] draws pause window onto gui *)
val draw_pause: t -> unit

(** [draw_background] draws background onto gui *)
val draw_background: t -> unit
*)
(** [test_img] draws image on canvas, testing purpose *)
val test_img : string -> t -> unit  