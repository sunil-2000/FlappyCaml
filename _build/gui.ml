open Graphics
open Camlimages
open Images
open Game 

let time_per_frame = 1. /. 30. 
let open_screen = Graphics.open_graph " 600x700";
  (* let animation_sequence = [] in *)

type t = {
  canvas_width : int; 
  canvas_height : int;
  camel_x : int;
  camel_y : int;
  pipe_x : int;
  camel_image : Graphics.image;
  camel_index : int; 
  camel_image_array: Graphics.image array;
  bottom_pipe_image: Graphics.image;
  top_pipe_image: Graphics.image;
  bottom_pipe_high_image : Graphics.image;
  top_pipe_high_image : Graphics.image;
  bottom_pipe_low_image : Graphics.image;
  top_pipe_low_image : Graphics.image;
  cactus_image : Graphics.image;
  pipe_num : int;
  ground_image: Graphics.image;
  pipe_type : int;
  player_score : int;
  highscore : int; 

}

let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
    let w = bitmap.Index8.width
    and h = bitmap.Index8.height
    and colormap = bitmap.Index8.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
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

(** [get_img] img takes the filename of the image as a ppm file and outputs
    a Graphics.image, which can be displayed with Graphics module. *)
let get_img img =
  Images.load img [] |> array_of_image |> make_image

let reg_top_pipe = get_img "assets/top.ppm"

let reg_bottom_pipe = get_img "assets/bottom.ppm"

let high_top_pipe = get_img "assets/top_high.ppm"

let high_bottom_pipe = get_img "assets/bottom_high.ppm"

let low_top_pipe = get_img "assets/top_low.ppm"

let low_bottom_pipe = get_img "assets/bottom_low.ppm"

let cactus = get_img "assets/cactus.ppm"

(* [make_player_array lst] constructs image array of player, used for
   animations *)
let make_player_array array = 
  Array.map get_img array 

let make_state wth hgt x y pipe_x pipe_type score index highscore = {
  canvas_width = wth; 
  canvas_height = hgt;
  camel_x = x;
  camel_y = y;
  pipe_x = pipe_x;
  camel_image = get_img "assets/clarkson.ppm";
  camel_index = index;
  camel_image_array = make_player_array 
      [| "assets/clarkson.ppm"; "assets/clarkson1.ppm"; "assets/clarkson2.ppm";
         "assets/clarkson3.ppm" |];
  bottom_pipe_image = reg_bottom_pipe;
  top_pipe_image = reg_top_pipe;
  bottom_pipe_high_image = high_bottom_pipe;
  top_pipe_high_image = high_top_pipe;
  bottom_pipe_low_image = low_bottom_pipe;
  top_pipe_low_image = low_top_pipe; 
  cactus_image = cactus; 
  pipe_num = 0;
  ground_image = get_img "assets/new_ground.ppm";
  pipe_type = pipe_type;
  player_score = score;
  highscore = highscore; 
}

let rec animate_player frame t = 
  if frame mod 5 = 0 then 
    (t.camel_index + 1) mod 4
  else t.camel_index

(* [update_fly y score index pipe pipe_type t] updates t appropriately when
   the state is fly (go) *)
let update_fly y score frame pipe_x pipe_type highscore t =
  {t with camel_y = y; player_score = score; camel_index = animate_player frame t; 
          pipe_x = pipe_x; pipe_type = pipe_type; highscore = highscore}

let update_run y score frame pipe_x t =
  {t with camel_y = y; player_score = score; camel_index = animate_player frame t;
          pipe_x = pipe_x; pipe_type = 1}

let draw_camel t =
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  if t.pipe_x < 250 && t.pipe_x > 100 then () else fill_rect 200 100 50 600;
  draw_image (t.camel_image_array.(t.camel_index)) t.camel_x t.camel_y

let draw_ground init = 
  draw_image init.ground_image 0 0;
  draw_image init.ground_image 190 0;
  draw_image init.ground_image 380 0;
  draw_image init.ground_image 570 0

let draw_back init = 
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 init.canvas_width init.canvas_height

let draw_pipe_helper_bottom init t = 
  match t with 
  | 0 -> draw_image init.bottom_pipe_image init.pipe_x 100
  | 1 -> draw_image init.bottom_pipe_high_image init.pipe_x 100
  | _ -> draw_image init.bottom_pipe_low_image init.pipe_x 100

let draw_pipe_helper_top init t =
  match t with 
  | 0 -> draw_image init.top_pipe_image init.pipe_x 500
  | 1 -> draw_image init.top_pipe_high_image init.pipe_x 575
  | _ -> draw_image init.top_pipe_low_image init.pipe_x 400

let draw_pipes init =
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 250 100 400 600;
  fill_rect 0 100 250 600;
  if init.pipe_x < 250 && init.pipe_x > 150 then fill_rect 200 100 50 600 else ();
  draw_pipe_helper_bottom init init.pipe_type;
  draw_pipe_helper_top init init.pipe_type

let draw_score init =
  let score_string = string_of_int init.player_score in
  moveto 520 620;
  set_text_size 500;
  set_color black;
  (*set_font "-*-Helvetica-medium-r-normal--50-*-*-*-*-*-iso8859-1";*)
  draw_string score_string;
  set_color white

let make_gui init = 
  draw_ground init;
  draw_pipes init;
  draw_camel init;
  draw_score init


let draw_pause =
  "fail"

let draw_gameover init = 
  Graphics.clear_graph ();
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 600 700;
  draw_image init.bottom_pipe_high_image 0 100;
  draw_image init.top_pipe_high_image 0 575;
  draw_image init.bottom_pipe_low_image 530 100;
  draw_image init.top_pipe_low_image 530 400;
  draw_ground init;
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
  draw_string "|_______||__| |__||_|   |_||_______|  |_______|  |___|  |_______||___|  |_|";
  moveto 260 275;
  draw_string "High Score: ";
  draw_string (string_of_int init.highscore); 
  set_text_size 50;
  let score_s = string_of_int init.player_score in 
  moveto 275 325;
  set_font "-*-Helvetica-medium-r-normal--80-*-*-*-*-*-iso8859-1";
  draw_string score_s;
  set_font "fixed"

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

let draw_start init =
  Graphics.clear_graph ();
  let light_blue = rgb 76 186 196 in
  set_color (light_blue);
  fill_rect 0 0 600 700;
  draw_image init.bottom_pipe_high_image 0 100;
  draw_image init.top_pipe_high_image 0 575;
  draw_image init.bottom_pipe_low_image 530 100;
  draw_image init.top_pipe_low_image 530 400;
  draw_ground init;
  set_color black;
  set_text_size 10;
  draw_flappycaml 120 500;
  draw_image init.camel_image 120 300;
  draw_image init.camel_image 220 300;
  draw_image init.camel_image 320 300;
  draw_image init.camel_image 420 300;
  moveto 240 400;
  set_color red;
  draw_string "Press any key to start";
  moveto 270 200;
  fill_rect 255 195 100 25; (* rectangle box that can be clicked *)
  set_color black;
  draw_string "Instructions";
  set_color white

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
  moveto 150 350;
  draw_string "Press 'q' to exit the game application.";
  set_color red; 
  fill_rect 450 50 100 50;
  set_color black;
  moveto 465 70;
  draw_string "start screen";
  set_color white; 
