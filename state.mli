(** [type t] is the type that is used to represent the state of the game *)
type t

(** Setters are used mainly for testing and debug purposes, and are not 
    needed to run the game  *)

(** [set_go t] sets the state of the game to "go" *)
val set_go: t -> t

(** [set_togo t] sets the state of the game to "togo" *)
val set_togo: t -> t 

(** [set_run t] sets the state of the game to "run"  *)
val set_run: t -> t

(** [set_torun t] sets the state of the game to "torun" *)
val set_torun: t -> t 

(** [set_gameover t] sets the state of the game to "gameover" *)
val set_gameover: t -> t 

(** [set_bomb t] sets the state of the game to "bomb" *)
val set_bomb: t -> t 

(** [set_tobomb t] sets the state of the game to "tobomb" *)
val set_tobomb: t -> t 

(** [set_death t] sets the state of the game to "death" *)
val set_death: t -> t

(** [string_of_state t] returns a string representation of t *)
val string_of_state : t -> string 

(** [check t] is used to check the current state of the game. [check t] is 
    responsible for transitioning the game to the correct state depending on
    the user's actions. *)
val check : t-> Game.t -> t

(** [make_state unit] initializes a state *)
val make_state : unit -> t
