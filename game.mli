(** Game module contains functions that the game relies on. For example, 
    collisions and movement are implemented in this module. *)


type t 

val jump: t -> unit 


val fall: t -> unit  



val run: t -> unit 

val fly: t -> unit 

val collide: t -> t 
