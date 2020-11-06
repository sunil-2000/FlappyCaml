
(** type [t] is an abstract type that represents the gui window. [t] 
    contains information about the width and height of the window.
    [t] also contains information for the images that will be displayed
    in the window. *)
type t

(** [make_gui] creates a graphical window based on the dimensions that are
    input. Opens graphical window and instantiates initial game canvas.
    Required: valid dimensions that can display on a user's screen. *)
val make_gui : t -> unit

(** [make_state] initializes window dimensions and starting coordinates of player *)
val make_state : int -> int -> int -> int -> int -> t

(** [draw_player] draws player onto the window *)
val draw_player : Game.t -> unit 

val gravity_draw: Game.t -> Game.t