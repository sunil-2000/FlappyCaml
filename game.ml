type t = {
  name : string;
  sprite : string;
  position : (float * float);
  orientation : float;
}

let accelerate pos = 
  let y = fst pos in 
