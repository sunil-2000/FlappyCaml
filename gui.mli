(** abstract type representing a gui record that contains information responsible
    for drawing the game properly on each update *)
type t

(** [update_fly int int int int t] updates t properly when in "fly" or "go" 
    state, so that on the next drawing the game is drawn properly *)
val update_fly: int -> int -> int -> int -> int -> int -> t -> t

(* (** [update_index t] updates the index of the animation array, so that the next
    image in the array is drawn on the next update *)
   val update_index: t -> int *)

(** [make_gui] makes the gui for the running of the game, building the ground,
    pipes, camel and score objects on the screen *)
val make_gui: t -> unit 

(** [make_state] instantiates the state type with all the required parameters *)
val make_state: int -> int -> int -> int -> int -> int -> int -> int -> int -> t

(* [update_run y score index t] updates t appropriately when the state is run *)
val update_run: int -> int -> int -> int -> int -> int -> t -> t


val update_torun: int -> int -> int -> int -> t -> t

val update_death: t -> int -> t

(**[draw_start] is used to draw the start screen when state is Start.
   it displays the sprite that the player will use and that the player should 
   click any key to start *)
val draw_start: t -> unit 

(**[draw_gameover] is used to draw the screen when state is GameOver.
   It displays that the game is over and the final score that the player achieved.
   It also offers an option for the player to restart the game. *)
val draw_gameover: t -> unit 

val draw_death: t -> unit

val draw_instructions: t -> unit 

val draw_run:  t -> unit 

val draw_sprites: t -> unit

val set_sprite: t -> int -> t 
