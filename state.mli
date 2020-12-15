type t

val string_of_state : t -> string 

val check : t-> Game.t -> t

val make_state : unit -> t
