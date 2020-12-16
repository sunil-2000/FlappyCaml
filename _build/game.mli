
(** Abstract type representing a player object*)
type t

type bomb = Bomb of int * int * bool | None 

type bomb_rec = {
  bombs : bomb list; 
  bomber_x : int;
} 


val get_bomb_rec: t -> bomb_rec

(** [create (a, b) c] returns a player type with position and velocity 
    specified. *)
val create: float * float -> float -> int -> string -> int option -> int -> int -> t

(** [get_pipe t] returns the x-coordinate of the bottom-left corner of a pipe.*)
val get_obs_x: t -> int 

val get_pipe_type: t -> int

val gravity_run: float -> t -> t 
(** [get_position t] returns the current coordinates of the player object as a 
    tuple.*)
val get_position: t -> float * float 

(** [set_can_jump t] sets the boolean field determining whether the player is 
    allowed to jump or not. *)
val set_can_jump: t -> bool -> t 

val set_obs_type: t -> string -> t 

(** [update (a, b) t] updates the parameters of the player to reflect changes 
    in game state due to gravity, jumping, score, collisions, and pipes. *)
val update: float -> t -> string -> t

(** [update_run t] updates parameters of the player to reflect changes that 
    occur in Run game state. *)
val update_run: float -> t -> t

val update_torun: float -> t -> t

val update_togo: float -> t -> t

val update_tobomb: float -> t -> t


val update_death: float -> t -> t

(** [get_y t] returns the y-coordinate of the player object. *)
val get_y: t -> float 

(** [get_pipe_type t] returns the type of pipe being drawn. *) 
val get_pipe_type: t -> int

(** [get_collision t] returns true if player collides with a pipe, false 
    otherwise. *)
val get_collision: t -> bool

(** [get_score t] returns the current score of the player. *)
val get_score: t -> int

val get_highscore: t ->  int 

val gravity_zero: float -> t -> t 

val get_bomber_x: t -> int

val get_highscore: t -> int 

val string_of_powerup: t -> string 

val int_of_powerup:  t -> int 

val get_pwr_pos : t -> int * int

val get_pwr_active : t -> bool 
