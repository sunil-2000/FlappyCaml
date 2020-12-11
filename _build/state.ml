open Game
open Graphics 
open Random 

type state = Start| GameOver | Go (* flying *) | Pause | Run | ToGo | ToRun

type t = {
  state : state; 
  score : int
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
    score = 0
  }

let get_state t = 
  t.state

let set_gameover state = 
  {state with state = GameOver}

let set_go state = 
  {state with state = Go}

let set_torun state = 
  {state with state = ToRun}

let set_pause state = 
  {state with state = GameOver}

let set_run state = 
  {state with state = Run}

let set_score value t = 
  {t with score = value}

let game_over player = 
  let y = snd (Game.get_position player) in 
  if y < 100. then 
    true 
  else 
    false

(* [check_mouse_click] returns true if a mouse click has occured *)
let rec check_key_click () = 
  let e = wait_next_event [Key_pressed] in
  if e.keypressed then true else check_key_click ()

(* [state_to_go] transitions the state from start to go *)
let state_to_go () = 
  (* dimensions of button <- should make field in gui for button *)
  if check_key_click () then 
    true 
  else 
    false

let switch state player =
  pick_interval player;
  if get_state state = Go then 
    {state with state = ToRun}
  else if get_state state = Run then
    {state with state = Go}
  else state
(* let pick_interval state player = 
   u.let interval = Random.int 10 + 3 *)

(* [check state player] returns the correct state of the game at given instance *)
let check state player = 
  if (Game.get_y player < 100. && get_state state = Go) || Game.get_collision player then 
    {state with state = GameOver}
  else if get_state state = Start then 
    if state_to_go () then 
      {state with state = Go}
    else 
      state 
      (* else if (Game.get_score player mod 3) = 0 then 
         switch state player     *)
  else if get_state state = GameOver then 
    if state_to_go () then 
      make_state ()
    else 
      state
  else 
    state
