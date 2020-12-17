open Game
open Graphics 
open Random 
(* GLOBAL VARIABLES FOR STATE *)
let old_score = ref 0 
let state_interval = ref 0

type t = 
  | Start
  | GameOver 
  | Go (* flying *) 
  | Pause 
  | Run 
  | Bomb 
  | ToBomb 
  | ToGo 
  | ToRun 
  | Death 
  | Instructions
  | Sprites
  | Sprite1 (* right now 1 represents clarkson, 2 = gries, 3 = camel *)
  | Sprite2 
  | Sprite3 
  | Dev
  (* add more states for game logic, then just pattern match against in 
  *)

let make_state () =  
  Start
(********************************SETTERS***************************************)
let set_gameover state = 
  GameOver

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

let game_over player = 
  let y = snd (Game.get_position player) in 
  if y < 100. then 
    true 
  else 
    false

(* [check_mouse_click] returns true if a mouse click has occured *)
let check_key_click () = 
  let e = wait_next_event [Key_pressed] in
  if e.keypressed then true else false 

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

(* return true if state should transition to instruction screen 
   xl = left boundary, xr = right boundary, yb = bottom boundary, 
   yt = top boundary *)
let check_to_transition xl xr yb yt = 
  match Graphics.mouse_pos () , Graphics.button_down () with 
  | (x, y), b -> 
    (* user is in the rectangle for button and has clicked *)
    if b && x > xl && x < xr && y > yb && y < yt then 
      true
    else 
      false  

(* transitions state appropriately if state = Start *)
let check_state_start state = 
  match Graphics.key_pressed (), check_to_transition 255 355 195 220, 
        check_to_transition 255 355 145 170, check_to_transition 255 355 95 120
  with 
  | true, _, _, _ -> Go 
  | _, true, _, _ -> Instructions
  | _, _, true, _ -> Sprites
  | _, _, _, true -> Dev
  | _, _, _, _ -> state  

(* transitions state appropriately if state = GameOver *)
let check_state_over state = 
  match check_key_click () with 
  | true -> Start
  | false -> state 

let check_instructions state = 
  match check_to_transition 450 550 50 100 with 
  | true -> Start
  | false -> state

let check_sprite_select state = 
  let sp1 = check_to_transition 170 220 300 350 in 
  let sp2 = check_to_transition 270 320 300 350 in 
  let sp3 = check_to_transition 370 420 300 350 in 
  match sp1, sp2, sp3 with 
  | true, _, _ -> Sprite1 (* clarkson *)
  | _, true, _ -> Sprite2 (* gries *)
  | _, _, true -> Sprite3 (* camel *)
  | _ -> state 

let check_sprites state = 
  match check_to_transition 450 550 50 100 with 
  | true -> Start
  | false -> check_sprite_select state

let check_go state player = 
  if (Game.get_player_y player < 100 && state = Go) 
  || Game.get_collision player then 
    Death
  else if Game.get_score player > 0 && Game.get_score player mod 2 = 0 
          && Game.get_score player <> !old_score then      
    switch state player    
  else 
    state 

let check_run state player = 
  if Game.get_collision player then 
    Death
  else if Game.get_score player > 0 && Game.get_score player mod 4 = 0 
          && Game.get_score player <> !old_score then 
    switch state player  
  else 
    state

let flush_kp () = 
  while Graphics.key_pressed () do
    let c = Graphics.read_key ()
    in ()
  done

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

let check_death state player = 
  if Game.get_player_y player <= -50 then 
    GameOver
  else 
    state  

let check_bomb state player = 
  if Game.get_collision player then 
    GameOver
  else if Game.get_bomber_x player < -240 then (* plane width = 240 *)
    ToRun
  else 
    state 

let check_dev state player = 
  match check_to_transition 450 550 50 100 with 
  | true -> Start
  | false -> state

(* [check state player] returns the correct state of the game at given instance *)
let check state player = 
  match state with 
  | GameOver -> check_state_over state 
  | Death -> check_death state player
  | Go -> check_go state player 
  | Start -> check_state_start state 
  | Run -> check_run state player 
  | Bomb -> check_bomb state player
  | Instructions -> check_instructions state
  | Sprites -> check_sprites state
  | Sprite1 | Sprite2 | Sprite3 -> state 
  | ToRun -> check_transition state player 
  | ToGo -> check_transition state player
  | ToBomb -> check_transition state player
  | Dev -> check_dev state player
  | _ -> failwith "not implmented in state.ml [check]"

let string_of_state t = 
  match t with 
  | Go -> "go"
  | Death -> "death"
  | GameOver -> "gameover"
  | Start -> "start"
  | Pause -> "pause"
  | Run -> "run"
  | Bomb -> "bomb"
  | ToGo -> "togo"
  | ToRun -> "torun"
  | ToBomb -> "tobomb"
  | Instructions -> "instructions"
  | Sprites -> "sprites"
  | Sprite1 -> "sprite1"
  | Sprite2 -> "sprite2"
  | Sprite3 -> "sprite3"
  | Dev -> "dev"
