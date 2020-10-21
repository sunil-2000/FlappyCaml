(** Game module contains functions that the game relies on. For example, 
    collisions and movement are implemented in this module. *)


type t 

(** [get_position] returns current coordinates of player. *)
val get_position : t -> (float * float)

val velocity_change : t -> unit

val calc_player_pos : t -> unit

