open Graphics
open Camlimages
open Images


type t = {width : int; height : int}

let make_gui width height = 
  {width = width; height = height}

(**let draw () = begin
   open_graph " 1000x750"; 

   plot 5 5;

   set_window_title "Flappy Caml"; 

   set_color (rgb 255 0 0);

   for y = 200 to 500 do 

    set_color (rgb 255 0 0);

    fill_circle x (y + 50) 40;

    Unix.sleepf 0.01;

    set_color (rgb 255 255 255);

    fill_circle x (y + 50) 40;
   done;
   end*)

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

let test_img file gui = 
  open_graph (" "^(string_of_int gui.width)^"x"^(string_of_int gui.width));
  let image = get_img file in
  draw_image image 0 0
