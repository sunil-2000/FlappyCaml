type t

val create: (float * float) -> float -> t

val is_gameover: t -> bool 

val get_velocity: t -> float

val get_pipe: t -> int 

val velocity_change: float -> t -> float

val get_position: t -> float * float 

val set_can_jump: t -> bool -> t 

val pipe_change: t -> t 

val gravity: float -> t -> t

val jump: t -> t

val update: float -> t -> t

val get_y: t -> float 