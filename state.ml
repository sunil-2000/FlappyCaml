open Game
open Graphics 

type state = Start| GameOver | Go | Pause | Run
type t = {
  state : state; 
  score : int
}

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
  let e = wait_next_event [Mouse_motion; Key_pressed] in
  if e.keypressed then true else check_key_click ()
(* [state_to_go] transitions the state from start to go *)
let state_to_go () = 
  match Graphics.mouse_pos () with 
  | (x, y) -> 
    (* dimensions of button <- should make field in gui for button *)
    if check_key_click () then 
      true 
    else 
      false

(* [check state player] returns the correct state of the game at given instance *)
let check state player = 
  if (Game.get_y player < 100. && get_state state = Go) || Game.get_collision player then 
    {state with state = GameOver}
  else if Game.get_score player = -10 then 
    set_run state 
  else if get_state state = Start then 
    if state_to_go () then 
      {state with state = Go}
    else 
      state 
  else 
    state
