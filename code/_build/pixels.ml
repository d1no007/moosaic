open Camlimages;;
open Images;;
open OImages;;
open Info;;
open Color;;
open PointTree;;

let average_color (filepath : string ref) (tile : rgb24 option) :
(Rgb.t * string ref) = 

  Bitmap.maximum_live := 0;

  let img = 
    (match tile with 
     | None ->  let file = !filepath in (OImages.rgb24 (OImages.load file [])) 
     | Some t ->  t) in 

  let width = img#width in 
  let height = img#height in 
  let pixels = width * height in 
  let color = ref {Rgb.r= 0; g = 0; b = 0;}  in
  for i = 0 to width - 1 do
     for j = 0 to height - 1 do
       color := Rgb.plus (!color) (img#get i j)
           done;
       done; 
  let new_color = {Rgb.r = ((!color).r / pixels); g = 
      ((!color).g / pixels); b = ((!color).b / pixels);} in
  (new_color, filepath) 
;; 

let avg_color_images = 
  let info = Array.to_list (Sys.readdir "outphotos") in
  let color_path = List.map (fun a -> 
    (average_color (ref ("outphotos/" ^ a)) None)) info in
   color_path
;;

let crop_resize (filepath : string ref) (s : int) : unit =

  let file = !filepath in
  let outfile = "out" ^ file in

  Printf.printf "%s \n" outfile ;

  (* Default value of 15000000, 60mb *)
  Bitmap.maximum_live := 0;
    
  let fmt, _ = Images.file_format file in
  let img = OImages.load file [] in
  let img = OImages.rgb24 img in 

  let square_crop i (w : int) (h : int) = 
    let diff = abs (w - h) in
      if w > h then i#sub (diff / 2) 0 (w - diff) h
      else i#sub 0 (diff / 2) w (h - diff) in

  let cropped = square_crop img img#width img#height in
    let new_img = cropped#resize None s s in
      new_img#save outfile (Some fmt) [Save_Quality 95]
;;

let crop_resize_all (s : int) = 
  let paths = Array.to_list (Sys.readdir "photos") in
    List.iter (fun x -> crop_resize (ref ("photos/" ^ x)) s) paths
;;

(* Takes in the number of tiles the user wants and the base image, 
 * and returns the tile dimensions (square), the number of tiles
 * across the width (based off user input), and the number of 
 * tiles across the height (based off user input). *)
let gridder (n : int) (filepath : string ref) : (int) = 

  let file = !filepath in 
  let outfile = "out" ^ file in

  (* Default value of 15000000, 60mb *)
  Bitmap.maximum_live := 0; 

  let fmt, _ = Images.file_format file in
  let img = OImages.rgb24 (OImages.load file []) in
  
  let small_side = if img#width > img#height then img#height 
    else img#width in
  let tile_side = truncate ((float small_side) /. (sqrt (float n))) in 
  Printf.printf "%d \n" tile_side;
  let new_width = tile_side * (img#width / tile_side) in
  Printf.printf "%d \n" new_width;
  let new_height = tile_side * (img#height / tile_side) in  
  Printf.printf "%d \n" new_height;

  let lost_pixels_width = img#width - new_width in
  let lost_pixels_height = img#height - new_height in
  Printf.printf "%d \n" lost_pixels_width; 
  Printf.printf "%d \n" lost_pixels_height;

  let bmp_crop (i : rgb24) (w : int) (h : int) (lpw : int) (lph : int) = 
    match lpw mod 2, lph mod 2 with
    | 0, 0 -> i#sub (lpw / 2) (lph / 2) (w - lpw) (h - lph)
    | 0, 1 -> i#sub (lpw / 2) (lph / 2) (w - lpw) (h - lph) 
    | 1, 0 -> i#sub (lpw / 2) (lph / 2) (w - lpw) (h - lph) 
    | 1, 1 -> i#sub (lpw / 2) (lph / 2) (w - lpw) (h - lph) 
    | _, _ -> failwith "Will not happen."
  in

  let bmp_cropped = 
    bmp_crop img img#width img#height lost_pixels_width lost_pixels_height in
  bmp_cropped#save outfile (Some fmt) [Save_Quality 95] ; tile_side 
;;


let blitz (img_tree : tree) (base_path : string ref) (s : int) : unit = 

  let file = !base_path in 
  let outfile = "out" ^ file in

  (* Default value of 15000000, 60mb *)
  Bitmap.maximum_live := 0; 

  let fmt, _ = Images.file_format file in
  let img = OImages.rgb24 (OImages.load file []) in
 
  let rec swapper s img_tree x y =
    let select_image (t: PointTree.tree) (elt: (PointTree.elt)) = 
      let (_, path) = PointTree.nearest_neighbor elt t in 
      let file = !path in
      let img = (OImages.rgb24 (OImages.load file [])) in img in
        if x <> img#width then 
          let lit = select_image img_tree (average_color (ref "") (Some (img#sub x y s s))) in
            (lit#blit 0 0 img x y s s; swapper s img_tree (x + s) y)
        else if y <> (img#height - s) then 
	  swapper s img_tree 0 (y + s)
        else () in

  swapper s img_tree 0 0 ;
  img#save outfile (Some fmt) [Save_Quality 95]  
;;  

