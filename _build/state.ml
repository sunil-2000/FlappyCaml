open Game

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


