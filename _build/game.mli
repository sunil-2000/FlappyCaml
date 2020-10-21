(** Game module contains functions that the game relies on. For example, 
    collisions and movement are implemented in this module. *)


type t 

val velocity_change : t -> unit

val calc_player_pos : t -> unit

