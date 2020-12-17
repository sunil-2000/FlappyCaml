type t

val set_go: t -> t

val set_togo: t -> t 

val set_run: t -> t

val set_torun: t -> t 

val set_gameover: t -> t 

val set_bomb: t -> t 

val set_tobomb: t -> t 

val set_death: t -> t

val string_of_state : t -> string 

val check : t-> Game.t -> t

val make_state : unit -> t
