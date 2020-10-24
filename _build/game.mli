(** Game module contains functions that the game relies on. For example, 
    collisions and movement are implemented in this module. *)

(** [type t] is an abstract type that represents a player *)
type t 

(** [create_t] creates a [player] with position and velocity specified by 
    floats. *)
val create_t : (float * float) -> float -> t

(** [get_position] returns current coordinates of player. *)
val get_position : t -> (float * float)

(** [get_velocity] returns current velocity of player*) 
val get_velocity: t -> float 

(** [gravity] applies gravity and velocity to a player and updates the 
    x and y coordinates appropiately. *)
val gravity : t -> t

(** [jump] returns player with updated player position and velocity values
    after player has jumped *)
val jump : t -> t 


