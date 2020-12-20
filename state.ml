open Game
open Graphics 
open Random 
(* GLOBAL VARIABLES FOR STATE *)
let old_score = ref 0 
let state_interval = ref 0

type t = 
  | Start
  | GameOver 
  | Go 
  | Run 
  | Bomb 
  | ToBomb 
  | ToGo 
  | ToRun 
  | Death 
  | Instructions
  | Easter
  | Sprites
  | Sprite1 
  | Sprite2 
  | Sprite3 
  | Dev
  | Quit 

let make_state () =  
  Start
(********************************SETTERS***************************************)
let set_gameover state = 
  GameOver

let set_quit state = 
  Quit 

let set_go state = 
  Go

let set_togo state = 
  ToGo

let set_torun state = 
  ToRun

let set_run state = 
  Run

let set_death state = 
  Death

let set_bomb state = 
  Bomb

let set_tobomb state = 
  ToBomb 

let set_instructions state = 
  Instructions
(******************************************************************************)

(* [check_mouse_click] returns true if a mouse click has occured *)
let check_key_click () = 
  let e = wait_next_event [Key_pressed] in
  if e.keypressed then true 
  else false 

let switch state player =
  Random.self_init (); 
  match state, Random.int 3 with 
  | Go, 0 -> Go
  | Run, 0 -> Run
  | Bomb, 0 -> ToGo
  | Go, 1 -> ToRun
  | Run, 1 -> ToGo
  | Bomb, 1 -> ToGo
  | Go, 2 -> ToBomb
  | Run, 2 -> ToGo
  | Bomb, 2 -> ToGo
  | _ -> failwith "switch"

(* [check_to_transition xl xr yb yt] returns true if the user's mouse is within
   the [xl, xr] and [yb yt] range and the user has clicked. 
   xl = left boundary, xr = right boundary, 
   yb = bottom boundary, yt = top boundary *)
let check_to_transition xl xr yb yt = 
  match Graphics.mouse_pos () , Graphics.button_down () with 
  | (x, y), b -> 
    (* user is in the rectangle for button and has clicked *)
    if b && x > xl && x < xr && y > yb && y < yt then 
      true
    else 
      false  

(* [check_state_start state] transitions state appropriately if state = Start.
   If the user presses any button the state switches to Go; the other state 
   transitions are determined if the user presses a specific area of the screen. *)
let check_state_start state = 
  match Graphics.key_pressed (), 
        check_to_transition 255 355 195 220, 
        check_to_transition 255 355 145 170, 
        check_to_transition 255 355 245 270, 
        check_to_transition 500 600 0 50 
  with 
  | true, _, _, _, _ -> Go 
  | _, true, _, _, _ -> Instructions
  | _, _, true, _, _ -> Sprites
  | _, _, _, true, _ -> Dev
  | _, _, _, _, true -> Quit 
  | _, _, _, _, _ -> state  

(* [check_state_over state] transitions state appropriately if state = GameOver *)
let check_state_over state = 
  match check_key_click () with 
  | true -> Start
  | _ -> state

(* [check_instructions state] transitions state appropriately if 
    state = Instructions*)
let check_instructions state = 
  match check_to_transition 450 550 50 100 with 
  | true -> Start
  | false -> state
(* [check_sprite_select state] is a helper function used to transition the state 
    appropriately if state = Sprite *)
let check_sprite_select state = 
  let sp1 = check_to_transition 170 220 300 350 in 
  let sp2 = check_to_transition 270 320 300 350 in 
  let sp3 = check_to_transition 370 420 300 350 in 
  match sp1, sp2, sp3 with 
  | true, _, _ -> Sprite1 
  | _, true, _ -> Sprite2 
  | _, _, true -> Sprite3 
  | _ -> state 

(* [check_sprites state] transitions state appropriately if state = Sprites *)
let check_sprites state = 
  match check_to_transition 450 550 50 100 with 
  | true -> Start
  | false -> check_sprite_select state

(* [check_go state player] transitions state appropriately if state = Go *)
let check_go state player = 
  if (Game.get_player_y player < 100 && state = Go) 
  || Game.get_collision player then 
    Death
  else if Game.get_score player > 0 && Game.get_score player mod 2 = 0 
          && Game.get_score player <> !old_score then      
    switch state player    
  else 
    state 

(* [check_run state player] transitions state appropriately if state = Run *)
let check_run state player = 
  if Game.get_collision player then 
    Death
  else if Game.get_score player > 0 && Game.get_score player mod 4 = 0 
          && Game.get_score player <> !old_score then 
    switch state player  
  else 
    state

(* [flush_kp ()] removes all the key presses in the queue *)
let flush_kp () = 
  while Graphics.key_pressed () do
    let c = Graphics.read_key ()
    in ()
  done

(* [check_transition state player] transitions state appropriately if state = 
   ToGo or ToRun or ToBomb *)
let check_transition state player =
  flush_kp ();
  old_score := Game.get_score player;
  match state with
  | ToGo ->
    if Game.get_player_y player >= 350 then
      Go
    else
      state
  | ToRun ->
    if Game.get_player_y player <= 100 then
      Run
    else
      state
  | ToBomb -> 
    if Game.get_obs_x player < 600 then 
      Bomb
    else 
      state
  | _ -> failwith "not a transition state [check_transition]"

(* [check_death state player] transitions state appropriately if state = Death *)
let check_death state player = 
  if Game.get_player_y player <= -50 then 
    GameOver
  else 
    state  

(* [check_bomb state player] transitions state appropriately if state = Bomb *)
let check_bomb state player = 
  if Game.get_collision player then 
    Death
  else if Game.get_bomber_x player < -240 then (* plane width = 240 *)
    ToRun
  else 
    state 

(* [check_dev state player] transitions state appropriately if state = Dev *)
let check_dev state player = 
  match check_to_transition 450 550 50 100, 
        check_to_transition 420 470 220 270  with 
  | true, _ -> Start
  | _, true -> Easter
  | _, _ -> state

(* [check_easter state player] transitions state appropriately if state = 
   Easter *)
let check_easter state player = 
  match check_to_transition 450 550 50 100 with 
  | true -> Start
  | false -> state 

(* [check_quit state player] transitions state appropriately if state = Quit *)
let check_quit state player = 
  Quit 

let check state player = 
  match state with 
  | GameOver -> check_state_over state 
  | Death -> check_death state player
  | Go -> check_go state player 
  | Start -> check_state_start state 
  | Run -> check_run state player 
  | Bomb -> check_bomb state player
  | Easter -> check_easter state player
  | Instructions -> check_instructions state
  | Sprites -> check_sprites state
  | Sprite1 | Sprite2 | Sprite3 -> state 
  | ToRun -> check_transition state player 
  | ToGo -> check_transition state player
  | ToBomb -> check_transition state player
  | Dev -> check_dev state player
  | Quit -> check_quit state player 

let string_of_state t = 
  match t with 
  | Go -> "go"
  | Death -> "death"
  | GameOver -> "gameover"
  | Start -> "start"
  | Run -> "run"
  | Bomb -> "bomb"
  | Easter -> "easter"
  | ToGo -> "togo"
  | ToRun -> "torun"
  | ToBomb -> "tobomb"
  | Instructions -> "instructions"
  | Sprites -> "sprites"
  | Sprite1 -> "sprite1"
  | Sprite2 -> "sprite2"
  | Sprite3 -> "sprite3"
  | Dev -> "dev"
  | Quit -> "quit"
