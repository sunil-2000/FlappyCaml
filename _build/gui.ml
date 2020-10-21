open Graphics
open Camlimages
open Images


let x = 300

let y = 200

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

let get_img img =
  Png.load img [] |> array_of_image |> make_image

let test_img = 
  open_graph " 600x400";
  let water = get_img "images/camel_test_sprite.png" in
  draw_image water 0 0;