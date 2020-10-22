open Graphics
open Camlimages
open Images


type t = {
  canvas_width : int; 
  canvas_height : int;
  camel_x : int;
  camel_y : int;
}

let make_state a b c d = {
  canvas_width = a; 
  canvas_height = b;
  camel_x = c;
  camel_y = d;
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

let draw_camel file t = 
  let image = get_img file in
  draw_image image t.camel_x t.camel_y

let draw_ground file = 
  let image = get_img file in
  draw_image image 0 0;
  draw_image image 190 0;
  draw_image image 380 0;
  draw_image image 570 0

let draw_back init = 
  set_color (rgb 91 164 238);
  fill_rect 0 0 init.canvas_width init.canvas_height

let draw_pipes file1 file2 = 
  let image1 = get_img file1 in 
  let image2 = get_img file2 in
  draw_image image1 300 100;
  draw_image image2 300 500

let make_gui init = 
  open_graph (" "^(string_of_int init.canvas_width)^"x"^(string_of_int init.canvas_height));
  draw_back init;
  draw_camel "assets/clarkson.ppm" init;
  draw_ground "assets/new_ground.ppm";
  draw_pipes "assets/bottom.ppm" "assets/top.ppm"