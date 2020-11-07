open Game

type state = GameOver | Go | Pause 
type t = {
  state : state; 
  score : int }

let make_state () =  
  {
    state = Go;
    score = 0
  }

let set_gameover state = 
  {state with state = GameOver}

let set_go state = 
  {state with state = Go}

let set_pause state = 
  {state with state = GameOver}

let set_score value t = 
  {t with score = value}

let game_over player = 
  let y = snd (Game.get_position player) in 
  if y < 100. then 
    true 
  else 
    false