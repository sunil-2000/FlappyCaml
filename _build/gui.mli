(** abstract type representing a gui record that contains information responsible
    for drawing the game properly on each update *)
type t

(** [update_fly int int int int t] updates t properly when in "fly" or "go" 
    state, so that on the next drawing the game is drawn properly *)
val update_fly: Game.t -> int -> t -> t

(* (** [update_index t] updates the index of the animation array, so that the next
    image in the array is drawn on the next update *)
   val update_index: t -> int *)

(** [make_gui] makes the gui for the running of the game, building the ground,
    pipes, camel and score objects on the screen *)

(** [make_state] instantiates the state type with all the required parameters *)
val make_state: int -> int -> int -> int -> int -> int  -> t

val draw_update: t -> string -> unit 
(* [update_run y score index t] updates t appropriately when the state is run *)
val update_run: Game.t -> int -> t -> t

val update_bomb: Game.t -> int ->  t -> t


val update_torun: Game.t -> int -> t -> t

val set_sprite: t -> int -> t 
