open Game
open Graphics 
open Random 

type state = 
  | Start
  | GameOver 
  | Go (* flying *) 
  | Pause 
  | Run 
  | ToGo 
  | ToRun 
  | Instructions
  | Sprites
  | Sprite1 (* right now 1 represents clarkson, 2 = gries, 3 = camel *)
  | Sprite2 
  | Sprite3 
  (* add more states for game logic, then just pattern match against in 
  *)

(* just make this variant type, not using record if only one field *)
type t = {
  state : state; 
}

let state_interval = ref 0

let pick_interval player = 
  let interval = (Random.int 1) + 1 in 
  let new_interval = interval + Game.get_score player in
  (*print_int new_interval;*)
  state_interval := new_interval 

let make_state () =  
  {
    state = Start;
  }

let get_state t = 
  t.state

let set_gameover state = 
  {state = GameOver}

let set_go state = 
  {state = Go}

let set_torun state = 
  {state = ToRun}

let set_pause state = 
  {state = GameOver}

let set_run state = 
  {state = Run}

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

(* switch moves state to transition *)
let switch state player =
  match Random.int 2 with 
  | 0 -> {state = ToRun}
  | 1 -> {state = ToGo}
  | _ -> failwith "switch"
(* let pick_interval state player = 
   u.let interval = Random.int 10 + 3 *)

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
  match Graphics.key_pressed (), check_to_transition 255 355 195 220, check_to_transition 255 355 145 170 with 
  | true, _, _ -> {state = Go} 
  | _, true, _ -> {state = Instructions}
  | _, _, true -> {state = Sprites}
  | _, _, _ -> state  

(* transitions state appropriately if state = GameOver *)
let check_state_over state = 
  match check_key_click () with 
  | true -> {state = Start}
  | false -> state 

let check_instructions state = 
  match check_to_transition 450 550 50 100 with 
  | true -> {state = Start}
  | false -> state

let check_sprite_select state = 
  let sp1 = check_to_transition 170 220 300 350 in 
  let sp2 = check_to_transition 270 320 300 350 in 
  let sp3 = check_to_transition 370 420 300 350 in 
  match sp1, sp2, sp3 with 
  | true, _, _ -> {state = Sprite1} (* clarkson *)
  | _, true, _ -> {state = Sprite2} (* gries *)
  | _, _, true -> {state = Sprite3} (* camel *)
  | _ -> state 

let check_sprites state = 
  match check_to_transition 450 550 50 100 with 
  | true -> {state = Start}
  | false -> check_sprite_select state

let check_go state player = 
  if (Game.get_y player < 100. && get_state state = Go) 
  || Game.get_collision player then 
    {state = GameOver}
  else if Game.get_score player mod 2 = 0 && Game.get_score player > 0 then 
    switch state player    
  else 
    state 

let check_run state player = 
  if Game.get_collision player then 
    {state = GameOver}
  else if Game.get_score player mod 2 = 0 && Game.get_score player > 0 then 
    switch state player  
  else 
    state

let check_torun state player = 
  if Game.get_y player = 100. then 
    {state = Run} 
  else 
    state

let check_togo state player = 
  if Game.get_y player >= 350. then 
    {state = Go} 
  else 
    state

(* [check state player] returns the correct state of the game at given instance *)
let check state player = 
  match get_state state with 
  | GameOver -> check_state_over state 
  | Go -> check_go state player 
  | Start -> check_state_start state 
  | Run -> check_run state player 
  | Instructions -> check_instructions state
  | Sprites -> check_sprites state
  | Sprite1 | Sprite2 | Sprite3 -> state 
  | ToRun -> check_torun state player
  | ToGo -> check_togo state player
  | _ -> failwith "not implmented in state.ml [check]"

let string_of_state t = 
  match t.state with 
  | Go -> "go"
  | GameOver -> "gameover"
  | Start -> "start"
  | Pause -> "pause"
  | Run -> "run"
  | ToGo -> "togo"
  | ToRun -> "torun"
  | Instructions -> "instructions"
  | Sprites -> "sprites"
  | Sprite1 -> "sprite1"
  | Sprite2 -> "sprite2"
  | Sprite3 -> "sprite3"