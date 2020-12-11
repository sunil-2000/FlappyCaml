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

let switch state player =
  pick_interval player;
  if get_state state = Go then 
    {state = Run}
  else if get_state state = Run then
    {state = Go}
  else state
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
  match Graphics.key_pressed (), check_to_transition 255 355 195 220 with 
  | true, true -> {state = Instructions}
  | false, true -> {state = Instructions}
  | true, false -> {state = Go} 
  | false, false -> state  

(* transitions state appropriately if state = GameOver *)
let check_state_over state = 
  match check_key_click () with 
  | true -> {state = Start}
  | false -> state 

let check_instructions state = 
  match check_to_transition 450 550 50 100 with 
  | true -> {state = Start}
  | false -> state

let check_go state player = 
  if (Game.get_y player < 100. && get_state state = Go) 
  || Game.get_collision player then 
    {state = GameOver}
  else if Game.get_score player mod 3 = 0 && Game.get_score player > 0 then 
    switch state player    
  else 
    state 
(* [check state player] returns the correct state of the game at given instance *)
let check state player = 
  match get_state state with 
  | GameOver -> check_state_over state 
  | Go -> check_go state player 
  | Start -> check_state_start state 
  | Run -> failwith "not implemented <- see [check] in state.ml"
  | Instructions -> check_instructions state 
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