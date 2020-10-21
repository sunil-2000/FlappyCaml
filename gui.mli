
(** type [t] is an abstract type that represents the gui window. [t] 
    contains information about the width and height of the window.
    [t] also contains information for the images that will be displayed
    in the window. *)
type t 

(** [make_gui] creates a graphical window based on the dimensions that are
    input. 
    Required: valid dimensions that can display on a user's screen. *)
val make_gui : int -> int -> t

val test_img : string -> t -> unit  