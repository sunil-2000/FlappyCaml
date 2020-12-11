(** Abstract type representing a player object*)
type t

(** [create (a, b) c] returns a player type with position and velocity 
    specified. *)
val create: (float * float) -> float -> int -> t

(** [is_gameover t] returns true if game over conditions are met, false 
    otherwise.*)
val is_gameover: t -> bool 

(** [get_velocity t] returns the velocity of a player object.*)
val get_velocity: t -> float

(** [get_pipe t] returns the x-coordinate of the bottom-left corner of a pipe.*)
val get_pipe: t -> int 

(** [get_position t] returns the current coordinates of the player object as a 
    tuple.*)
val get_position: t -> float * float 

(** [set_can_jump t] sets the boolean field determining whether the player is 
    allowed to jump or not. *)
val set_can_jump: t -> bool -> t 

(** [pipe_change t] updates the position of the pipes. *)
val pipe_change: t -> t 

(** [gravity (a, b) p] returns a player object with the position updated to 
    reflect changes due to gravity, velocity, and acceleration. *)
val gravity: float -> t -> t

(** [jump t] modifies player velocity to apply a jump to the player model. *)
val jump: t -> t

(** [update (a, b) t] updates the parameters of the player to reflect changes 
    in game state due to gravity, jumping, score, collisions, and pipes. *)
val update: float -> t -> t

(** [update_run t] updates parameters of the player to reflect changes that 
    occur in Run game state. *)
val update_run: float -> t -> t

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
(** [get_score_updated t] returns true if player score has been updated since
    passing the most recent pipe, false otherwise. *)
val get_score_updated: t -> bool
