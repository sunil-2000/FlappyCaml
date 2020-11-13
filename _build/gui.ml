open Graphics
open Camlimages
open Images
open Game 

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
  camel_image_array: Graphics.image list ;
  bottom_pipe_image: Graphics.image;
  top_pipe_image: Graphics.image;
  bottom_pipe_high_image : Graphics.image;
  top_pipe_high_image : Graphics.image;
  bottom_pipe_low_image : Graphics.image;
  top_pipe_low_image : Graphics.image;
  pipe_num : int;
  ground_image: Graphics.image;
  pipe_type : int;
  player_score : int;

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

(* [make_player_array lst] constructs image array of player, used for
   animations *)
let make_player_array lst = 
  List.map get_img lst 

let make_state wth hgt x y pipe_x pipe_type score index = {
  canvas_width = wth; 
  canvas_height = hgt;
  camel_x = x;
  camel_y = y;
  pipe_x = pipe_x;
  camel_image = get_img "assets/clarkson.ppm";
  camel_index = index;
  camel_image_array = make_player_array 
      ["assets/clarkson.ppm"; "assets/clarkson1.ppm"; "assets/clarkson2.ppm";
       "assets/clarkson3.ppm"];
  bottom_pipe_image = reg_bottom_pipe;
  top_pipe_image = reg_top_pipe;
  bottom_pipe_high_image = high_bottom_pipe;
  top_pipe_high_image = high_top_pipe;
  bottom_pipe_low_image = low_bottom_pipe;
  top_pipe_low_image = low_top_pipe; 
  pipe_num = 0;
  ground_image = get_img "assets/new_ground.ppm";
  pipe_type = pipe_type;
  player_score = score;
}

(* [update_fly y score index pipe pipe_type t] updates t appropriately when
   the state is fly (go) *)
let update_fly y score index pipe_x pipe_type t =
  {t with camel_y = y; player_score = score; camel_index = index; 
          pipe_x = pipe_x; pipe_type = pipe_type}

(* [update_run y score index t] updates t appropriately when the state is run *)
let update_run y score index t =
  {t with camel_y = y; player_score = score; camel_index = index}

(* [animate_player] changes image of character to create animation 
   effect *)
let rec animate_player index t = 
  match t with 
  | [] -> failwith "Impossible"
  | h::t -> if index = 0 then h else animate_player (index-1) t 

(* [update_index t] updates index of camel_image_array *)
let update_index t  = 
  let index = t.camel_index in 
  if index = 3 then 0 else t.camel_index + 1

let draw_camel t =
  set_color (white);
  if t.pipe_x < 250 && t.pipe_x > 100 then () else fill_rect 200 100 50 600;
  draw_image (animate_player t.camel_index t.camel_image_array) t.camel_x t.camel_y

let draw_ground init = 
  draw_image init.ground_image 0 0;
  draw_image init.ground_image 190 0;
  draw_image init.ground_image 380 0;
  draw_image init.ground_image 570 0

let draw_back init = 
  set_color (rgb 91 164 238);
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
  set_font "-*-Helvetica-medium-r-normal--80-*-*-*-*-*-iso8859-1";
  draw_string score_string;
  set_color white

let make_gui init = 
  draw_ground init;
  draw_pipes init;
  draw_camel init;
  draw_score init

let draw_pause =
  "fail"

let draw_gameover = 
  "fail"

let draw_start init =
  Graphics.clear_graph ();
  set_color black;
  draw_string "Flappy";
  draw_string "Caml";
  draw_image init.camel_image 250 300;
  let button = "Click anywhere to start" in
  moveto 200 400;
  draw_string button;
  set_color white
