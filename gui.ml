open Graphics
open Camlimages
open Images
open Game 

(* [open_screen] opens the graphics window *)
let open_screen = Graphics.open_graph " 600x700"


(* [type pipe_array] represents an array of Pipes that contains tuples as 
     each element. Each tuple represents a set of pipes, with the given images
     and y coordinates to draw the images at *)
type pipe_array = ((Graphics.image * (int)) * (Graphics.image * (int))) array

type t = {
  canvas_width : int; 
  canvas_height : int;
  player : Game.t; 
  camel_image : Graphics.image;
  camel_index : int; 
  camel_image_array: Graphics.image array;
  pipe_array : pipe_array; 
  cactus_image : Graphics.image;
  powerup_image : Graphics.image;
  ground_image: Graphics.image;
}

(* [array_of_image] takes in a ppm file and returns a color array array
   so that it can be converted to an image and displayed
   Note: this function used to be part of the camlimages library, but was
   deprecated, so we added its implementation here. Sourced from:
   https://github.com/jrk/camlimages/blob/master/src/graphic_image.ml *)
let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
    let w = bitmap.Index8.width
    and h = bitmap.Index8.height
    and colormap = bitmap.Index8.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) 
        colormap in
    if bitmap.Index8.transparent <> -1 then
      cmap.(bitmap.Index8.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
    let w = bitmap.Index16.width
    and h = bitmap.Index16.height
    and colormap = bitmap.Index16.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
    if bitmap.Index16.transparent <> -1 then
      cmap.(bitmap.Index16.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
    let w = bitmap.Rgb24.width
    and h = bitmap.Rgb24.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
            rgb r g b))
  | Rgba32 _ -> failwith "RGBA not supported"
  | Cmyk32 _ -> failwith "CMYK not supported"

(* [get_img] img takes the filename of the image as a ppm file and outputs
    a Graphics.image, which can be displayed with Graphics module. *)
let get_img img =
  Images.load img [] |> array_of_image |> make_image

let reg_top_pipe = get_img "assets/top.ppm"

let reg_bottom_pipe = get_img "assets/bottom.ppm"

let high_top_pipe = get_img "assets/top_high.ppm"

let high_bottom_pipe = get_img "assets/bottom_high.ppm"

let low_top_pipe = get_img "assets/top_low.ppm"

let low_bottom_pipe = get_img "assets/bottom_low.ppm"

let cactus = get_img "assets/cactus.pbm"

let bird = get_img "assets/bird_1.pbm"

let dino = get_img "assets/dino_1.pbm"

let camel = get_img "assets/camel_1.pbm"

let death = get_img "assets/deathimage.pbm"

let mushroom = get_img "assets/mario_mushroom.pbm"

let bomb = get_img "assets/bomb.ppm"

let bomber = get_img "assets/bomber.ppm"

let turtle = get_img "assets/turtle.pbm"

let star = get_img "assets/star.pbm"

let sunil = get_img "assets/sunil.ppm"

let rishik = get_img "assets/rishik.ppm"

let brian = get_img "assets/brian.ppm"

let udai = get_img "assets/udai.ppm"

(* [make_player_array lst] constructs image array of player, used for
   animations *)
let make_player_array array = 
  Array.map get_img array 

(* GLOBAL VARIABLES *)
let bird_array = make_player_array 
    [| "assets/bird_1.pbm"; 
       "assets/bird_2.pbm";
       "assets/bird_3.pbm" |]

let camel_array = make_player_array 
    [|"assets/camel_1.pbm"; 
      "assets/camel_2.pbm"; 
      "assets/camel_3.pbm"|]

let dino_array = make_player_array 
    [|"assets/dino_1.pbm"; 
      "assets/dino_2.pbm";
      "assets/dino_3.pbm";
      "assets/dino_4.pbm";
      "assets/dino_5.pbm";
      "assets/dino_6.pbm"|]

let death_array = make_player_array [|"assets/deathimage.pbm"|]

let sprite_arrays = [|bird_array; 
                      dino_array; 
                      camel_array; 
                      death_array|]

let sprites = [|bird; 
                dino; 
                camel; 
                death|]

let powerup_array = [|star; turtle|] 

let pipe_array = [|((reg_top_pipe, 500), (reg_bottom_pipe, 100)); 
                   ((high_top_pipe, 575), (high_bottom_pipe, 100)); 
                   ((low_top_pipe, 400), (low_bottom_pipe, 100))|]

(* [make_state] takes in a player and initializes all the fields needed to
   draw the game on the screen, including the background, player, and sprite
   values *)
let make_state player = {
  canvas_width = 600; 
  canvas_height = 700;
  player = player; 
  camel_image = camel;
  pipe_array = pipe_array;
  camel_image_array = camel_array; 
  camel_index = 0;
  cactus_image = cactus; 
  powerup_image = mushroom;
  ground_image = get_img "assets/new_ground.ppm";
}

(* [set_sprite] takes in a gui type and an index to set the sprite image
   based on the player's selection for a particular game *)
let set_sprite t image_array_no = 
  {t with 
   camel_image_array = sprite_arrays.(image_array_no - 1);
   camel_image =  sprites.(image_array_no - 1)}

(* [animate_player] returns an index in order to change the sprite image index 
   for each frame of the animation (every 5 frames here) *)
let rec animate_player frame t =  
  if frame mod 5 = 0 then 
    (t.camel_index + 1) mod Array.length t.camel_image_array
  else 
    t.camel_index

(* [update_fly] updates t appropriately when the state is fly (go), 
   if draw = -1, then powerup = None *)
let update_death player t = 
  {t with player = player}

(* [update_dynamic] updates the camel sprite image by calling [animate_player]
   at each frame to change the sprite image for the player animation *)
let update_dynamic player frame t = 
  {t with 
   player = player;
   camel_index = animate_player frame t;
  }

(* [draw_camel] retrieves the player's x and y position from the Game module
   and draws the player based on the inputted sprite and frame of its 
   animation *)
let draw_camel t =
  let x = Game.get_player_x t.player in 
  let y = Game.get_player_y t.player in 
  draw_image (t.camel_image_array.(t.camel_index)) x y 

(* [draw_death_img] retrieves the player's x and y position from the Game module
   erases the player and draws a death symbol over the newly erased area *)
let draw_death_img t =
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  let x = Game.get_player_x t.player in 
  let y = Game.get_player_y t.player in 
  draw_image death x y 

(* [draw_ground] paints the ground onto the canvas with the given ground
   image *)
let draw_ground init = 
  draw_image init.ground_image 0 0;
  draw_image init.ground_image 190 0;
  draw_image init.ground_image 380 0;
  draw_image init.ground_image 570 0

(* [draw_back] paints the background onto the canvas in blue *)
let draw_back init = 
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 init.canvas_width init.canvas_height

(* [draw_pipes] retrieves the pipe pair images from the Game module 
   (high, low, normal height) and their x position as they move across the 
   screen then paints them onto the canvas *)
let draw_pipes init = 
  let pipe_type = Game.get_obs_type init.player in 
  match init.pipe_array.(pipe_type) with 
  | ((top , y), (bottom, y')) -> 
    let x = Game.get_obs_x init.player in 
    draw_image bottom x y';
    draw_image top x y

(* [draw_cactus] retrives the x position of the cactus from the Game module 
   as it moves across the screen and paints it onto the canvas *)
let draw_cactus init = 
  draw_image cactus (Game.get_obs_x init.player) 100

(* [draw_score] retrives the score from the Game module
   and paints it onto the top right corner of the canvas *)
let draw_score init =
  let score_string = string_of_int (Game.get_score init.player) in
  moveto 520 620;
  set_text_size 500;
  set_color black;
  draw_string score_string;
  set_color white

(* [draw_powerups] retrives the index of the powerup image from the Game module
   and draws it on the screen if it has been generated and if it is collected
   by the player then it paints it in the bottom left corner of the canvas *)
let draw_powerups init = 
  let index = Game.int_of_powerup init.player in 
  if index <> -1 then 
    let (x, y) = 
      if Game.get_pwr_active init.player then 
        0, 0 
      else 
        let x', y' = Game.get_pwr_pos init.player in 
        x', y' in 
    draw_image powerup_array.(index) x y 
  else 
    let lightblue = rgb 76 186 196 in
    set_color lightblue;
    fill_rect 35 35 0 0

(* [draw_bomber] retrieves the bomber's x position from the Game module as it
   moves across the screen and paints it onto the canvas *)
let draw_bomber init = 
  draw_image bomber (Game.get_bomber_x init.player) 500 

(* [draw_bomb_ob_aux] takes in a bomb list and recurses through the list,
   painting each bomb onto the canvas if it exists and has been dropped *)
let rec draw_bomb_ob_aux lst =  
  match lst with 
  | h::t ->
    begin 
      match h with 
      | None -> (); draw_bomb_ob_aux t 
      | Bomb (x, y, b) -> 
        begin 
          match b with 
          | true -> draw_image bomb x y; draw_bomb_ob_aux t 
          | false ->  draw_bomb_ob_aux t 
        end 
    end 
  | [] -> ()

(* [draw_bomb_ob] retrieves the bomb list from the Game module and uses 
   [draw_bomb_ob_aux] to paint all the dropped bombs onto the canvas *)
let draw_bomb_ob init = 
  draw_bomb_ob_aux (Game.get_bombs_list init.player)

(* [draw_fly] draws the fly state by painting the background, ground, pipes, 
   camel, powerups, and score (in that order to prevent masking) *)
let draw_fly init = 
  draw_back init; 
  draw_ground init;
  draw_pipes init;
  draw_camel init;
  draw_powerups init;
  draw_score init

(* [draw_run] draws the run state by painting the background, ground, cactus, 
   camel, powerups, and score (in that order to prevent masking) *)
let draw_run init = 
  draw_back init; 
  draw_ground init;
  draw_cactus init;
  draw_camel init; 
  draw_powerups init;
  draw_score init

(* [draw_bomb] draws the bomb state by painting the background, ground, 
   camel, bombs, bomber, and score (in that order to prevent masking) *)
let draw_bomb init = 
  draw_back init;
  draw_ground init;
  draw_camel init;
  draw_bomb_ob init;
  draw_bomber init;
  draw_score init

(* [draw_death] draws a dead player by using [draw_death_img] *)
let draw_death init =
  draw_death_img init

(* [draw_gameover_ascii] draws the gameover ascii art*)
let draw_gameover_ascii init = 
  set_color black;
  moveto 80 500;
  draw_string " _______  _______  __   __  _______    _______  __   __  _______  ______   ";
  moveto 80 490;
  draw_string "|       ||   _   ||  |_|  ||       |  |       ||  | |  ||       ||    _ |  ";
  moveto 80 480;
  draw_string "|    ___||  |_|  ||       ||    ___|  |   _   ||  |_|  ||    ___||   | ||  ";
  moveto 80 470;
  draw_string "|   | __ |       ||       ||   |___   |  | |  ||       ||   |___ |   |_||_ ";
  moveto 80 460;
  draw_string "|   ||  ||       ||       ||    ___|  |  |_|  ||       ||    ___||    __  |";
  moveto 80 450;
  draw_string "|   |_| ||   _   || ||_|| ||   |___   |       | |     | |   |___ |   |  | |";
  moveto 80 440;
  draw_string "|_______||__| |__||_|   |_||_______|  |_______|  |___|  |_______||___|  |_|"

(* [draw_gameover] draws the gameover screen*)
let draw_gameover init = 
  Graphics.clear_graph ();
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 600 700;
  draw_image high_bottom_pipe 0 100;
  draw_image high_top_pipe 0 575;
  draw_image low_bottom_pipe 530 100;
  draw_image low_top_pipe 530 400;
  draw_ground init;
  draw_gameover_ascii init; 
  moveto 260 275;
  draw_string "High Score: ";
  draw_string (string_of_int (Game.get_highscore init.player)); 
  set_text_size 50;
  let score_s = string_of_int (Game.get_score init.player) in 
  moveto 275 325;
  set_font "-*-Helvetica-medium-r-normal--80-*-*-*-*-*-iso8859-1";
  draw_string score_s;
  set_font "fixed"

(* [draw_flappycaml] draws the flappy caml ascii art at x y (left top corner) *)
let draw_flappycaml x y = 
  moveto x y;
  draw_string "  _____ _                            ____                _ ";
  moveto x (y -10);
  draw_string " |  ___| | __ _ _ __  _ __  _   _   / ___|__ _ _ __ ___ | |";
  moveto x (y-20);
  draw_string " | |_  | |/ _` | '_ \| '_ \| | | | | |   / _` | '_ ` _ \| |";
  moveto x (y-30);
  draw_string " |  _| | | (_| | |_) | |_) | |_| | | |__| (_| | | | | | | |";
  moveto x (y-40);
  draw_string " |_|   |_|\__,_| .__/| .__/ \__, |  \____\__,_|_| |_| |_|_|";
  moveto x (y-50);
  draw_string "               |_|   |_|    |___/                          "

(* [draw_camel_ascii] draws the camel ascii art at x y (left top corner) *)
let draw_camel_ascii init x y =
  moveto x y;
  draw_string "                    ,,__                               ";
  moveto x (y-10);
  draw_string "          ..  ..   / o._)                   .---.      ";
  moveto x (y-20);
  draw_string "          /--'/--\  \-'||        .----.    .'     '.   ";
  moveto x (y-30);
  draw_string "         /        \_/ / |      .'      '..'         '-.";
  moveto x (y-40);
  draw_string "       .'\  \__\  __.'.'     .'          -._           ";
  moveto x (y-50);
  draw_string "         )\ |  )\ |      _.'                           ";
  moveto x (y-60);
  draw_string "        // \\ // \\                                    ";
  moveto x (y-70);
  draw_string "       ||_  \\|_  \\_                                  ";
  moveto x (y-80);
  draw_string "       '--' '--'' '--'                                 "  

(* Camel ascii art citation: 
   Thank you for visiting https://asciiart.website/
   This ASCII pic can be found at
   https://asciiart.website/index.php?art=animals/camels *)

(* [draw_developers_ascii] draws developers ascii art at x y (left top corner) *)
let draw_developers_ascii x y = 
  moveto x y;
  draw_string " ______   _______  __   __  _______  ___      _______  _______  _______  ______    _______ ";
  moveto x (y - 10);
  draw_string "|      | |       ||  | |  ||       ||   |    |       ||       ||       ||    _ |  |       |";   
  moveto x (y - 20);
  draw_string "|  _    ||    ___||  |_|  ||    ___||   |    |   _   ||    _  ||    ___||   | ||  |  _____|"; 
  moveto x (y - 30);
  draw_string "| | |   ||   |___ |       ||   |___ |   |    |  | |  ||   |_| ||   |___ |   |_||_ | |_____ ";
  moveto x (y - 40);
  draw_string "| |_|   ||    ___||       ||    ___||   |___ |  |_|  ||    ___||    ___||    __  ||_____  |";
  moveto x (y - 50);
  draw_string "|       ||   |___  |     | |   |___ |       ||       ||   |    |   |___ |   |  | | _____| |";
  moveto x (y - 60);
  draw_string "|______| |_______|  |___|  |_______||_______||_______||___|    |_______||___|  |_||_______|"

(* [draw_start] draws the start screen *)
let draw_start init =
  Graphics.clear_graph ();
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 600 700;
  draw_image high_bottom_pipe 0 100;
  draw_image high_top_pipe 0 575;
  draw_image low_bottom_pipe 530 100;
  draw_image low_top_pipe 530 400;
  draw_ground init;
  set_color black;
  set_text_size 10;
  draw_flappycaml 120 500;
  draw_image init.camel_image 280 300;
  moveto 240 400;
  set_color red;
  draw_string "Press any key to start";
  fill_rect 255 195 100 25; (* rectangle box that can be clicked *)
  fill_rect 255 140 100 25; 
  fill_rect 255 245 100 25;
  fill_rect 525 0 75 50;  
  set_color white;
  moveto 293 252;
  draw_string "Devs";
  moveto 270 202;
  draw_string "Instructions";
  moveto 283 147;
  draw_string "Sprites";
  moveto 550 15;
  draw_string "QUIT";
  set_font "fixed"

(* [draw_instructions] draws the instructions screen *)
let draw_instructions init = 
  Graphics.clear_graph ();
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 600 700;
  set_color black; 
  draw_flappycaml 120 500;
  moveto 150 400;
  draw_string "Press spacebar to jump through the obstacles when flying";
  moveto 150 380;
  draw_string "and over the obstacles when running.";
  moveto 150 360;
  draw_string "If you touch a star or turtle, the player will have a status";
  moveto 150 340;
  draw_string "effect. A star grants invincibility, while the turtle slows";
  moveto 150 320;
  draw_string "the player down.";
  draw_camel_ascii init 100 300;
  set_color red; 
  fill_rect 450 50 100 50;
  moveto 465 70;
  set_color white;
  draw_string "start screen"

(* [draw_sprites] draws the sprites screen *)
let draw_sprites init = 
  Graphics.clear_graph ();
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 600 700;
  set_color black;
  draw_image bird 170 300;
  draw_image dino 270 300;
  draw_image camel 370 300;
  set_color red; 
  fill_rect 450 50 100 50;
  set_color white;
  moveto 465 70;
  draw_string "start screen"

(* [draw_developers] draws the developers screen *)
let draw_developers init = 
  Graphics.clear_graph ();
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 600 700;
  set_color black;
  moveto 150 500;
  draw_developers_ascii 25 500;
  moveto 200 320;
  draw_string "Made with <3 by:";
  moveto 200 300;
  draw_string "Sunil, Rishik, Brian, and Udai";
  draw_image sunil 120 220;
  draw_image rishik 220 220;
  draw_image udai 320 220;
  draw_image brian 420 220;
  set_color red; 
  fill_rect 450 50 100 50;
  moveto 465 70;
  set_color white;
  draw_string "start screen"

(* [draw_our_story] draws the our story ascii art at x y (top left corner) *)
let draw_our_story x y = 
  moveto x y;
  draw_string " _______  __   __  ______      _______  _______  _______  ______    __   __ ";
  moveto x (y - 10);
  draw_string "|       ||  | |  ||    _ |    |       ||       ||       ||    _ |  |  | |  |";
  moveto x (y - 20);
  draw_string "|   _   ||  | |  ||   | ||    |  _____||_     _||   _   ||   | ||  |  |_|  |";
  moveto x (y - 30);
  draw_string "|  | |  ||  |_|  ||   |_||_   | |_____   |   |  |  | |  ||   |_||_ |       |";
  moveto x (y - 40);
  draw_string "|  |_|  ||       ||    __  |  |_____  |  |   |  |  |_|  ||    __  ||_     _|";
  moveto x (y - 50);
  draw_string "|       ||       ||   |  | |   _____| |  |   |  |       ||   |  | |  |   |  ";
  moveto x (y - 60);
  draw_string "|_______||_______||___|  |_|  |_______|  |___|  |_______||___|  |_|  |___|  "

(* [draw_easter] draws the easter egg when a user clicks on Brian's face *)
let draw_easter init = 
  Graphics.clear_graph ();
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 600 700;
  set_color black;
  draw_our_story 10 500;
  moveto 100 400;
  draw_string "Python. Java. C++. OCaml.";
  moveto 100 390;
  draw_string "Long ago, the four languages lived together in harmony.";
  moveto 100 380;
  draw_string "Then everything changed when the OCaml language attacked.";
  moveto 100 370;
  draw_string "Only the Programmer, master of all four languages could stop it";
  moveto 100 360;
  draw_string "But when the world needed him most, he logged off.";
  moveto 100 350;
  draw_string "A semester passed and my brother and I discovered a new Programmer,"; 
  moveto 100 340;
  draw_string "an OCaml programmer named Brian.";
  moveto 100 330;
  draw_string "and although his OCaml skills are great, he still has a lot to learn";
  moveto 100 320;
  draw_string "before he can code anything.";
  moveto 100 310;
  draw_string "But I believe Brian can code this project.";
  set_color red; 
  fill_rect 450 50 100 50;
  moveto 465 70;
  set_color white;
  draw_string "start screen"


(* [draw_update] is reponsible for drawing the correct frame, which
   is dependent upon [state] that is represented by a string *)
let draw_update init state = 
  match state with 
  | "go" -> draw_fly init
  | "run" -> draw_run init 
  | "bomb" -> draw_bomb init
  | "death" -> draw_death init 
  | "gameover" -> draw_gameover init 
  | "sprites" -> draw_sprites init 
  | "instructions" -> draw_instructions init
  | "dev" -> draw_developers init 
  | "start" -> draw_start init 
  | "torun" -> draw_fly init 
  | "togo" -> draw_fly init 
  | "tobomb" -> draw_fly init
  | "easter" -> draw_easter init
  | _ -> failwith "draw for this state not impl [draw_update]"



