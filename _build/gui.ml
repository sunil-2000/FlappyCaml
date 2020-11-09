open Graphics
open Camlimages
open Images
open Game 

let open_screen = Graphics.open_graph " 600x700";

type t = {
  canvas_width : int; 
  canvas_height : int;
  camel_x : int;
  camel_y : int;
  pipe_x : int;
  camel_image : Graphics.image;
  bottom_pipe_image: Graphics.image;
  top_pipe_image: Graphics.image;
  bottom_pipe_high_image : Graphics.image;
  top_pipe_high_image : Graphics.image;
  pipe_num : int;
  ground_image: Graphics.image;
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

let make_state a b c d e = {
  canvas_width = a; 
  canvas_height = b;
  camel_x = c;
  camel_y = d;
  pipe_x = e;
  camel_image = get_img "assets/clarkson.ppm";
  bottom_pipe_image = reg_bottom_pipe;
  top_pipe_image = reg_top_pipe;
  bottom_pipe_high_image = high_bottom_pipe;
  top_pipe_high_image = high_top_pipe; 
  pipe_num = 0;
  ground_image = get_img "assets/new_ground.ppm"
}

let draw_camel t =
  set_color (white);
  if t.pipe_x < 250 && t.pipe_x > 100 then () else fill_rect 200 100 50 600;
  draw_image t.camel_image t.camel_x t.camel_y

let draw_ground init = 
  draw_image init.ground_image 0 0;
  draw_image init.ground_image 190 0;
  draw_image init.ground_image 380 0;
  draw_image init.ground_image 570 0

let draw_back init = 
  set_color (rgb 91 164 238);
  fill_rect 0 0 init.canvas_width init.canvas_height

let draw_pipes init =
  fill_rect 250 100 400 600;
  fill_rect 0 100 250 600;
  if init.pipe_x < 250 && init.pipe_x > 150 then fill_rect 200 100 50 600 else ();
  draw_image init.bottom_pipe_image init.pipe_x 100;
  draw_image init.top_pipe_image init.pipe_x 500

let make_gui init = 
  draw_ground init;
  draw_pipes init;
  draw_camel init

(* helper function for move_player, responsible for gravity drawing *)
let gravity_draw t_delta player = 
  match Game.get_position player with
  |(x,y) ->  
    let a = int_of_float x in
    let b = int_of_float y in
    let pipe_x = Game.get_pipe player in
    let test = make_state 600 700 a b pipe_x in
    make_gui test;
    Unix.sleepf 0.001;
    Game.gravity t_delta player

(* [jump_draw player] is a helper function for [move_player] responsible for 
   drawing jump movement *)
let jump_draw player =
  let key = Graphics.wait_next_event [Key_pressed] in
  if key.keypressed then 
    Game.jump player
  else 
    player

let pipe_change player = 
  Game.pipe_change player
  |> Game.get_pipe

let draw_player t_delta player = 
  match Game.get_position player with 
  |(x,y) -> 
    if 
      (Graphics.key_pressed ()) && (Graphics.read_key () = 'v') then 
      gravity_draw t_delta (Game.jump player) 
    else 
      gravity_draw t_delta player                                                                                              
