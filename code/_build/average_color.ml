open Images
open OImages
open Info
open Color 

let average_color (filepath : string ref) : Rgb.t * string ref = 

  let file = !filepath in

  (* Default value of 15000000, 60mb *)
  Bitmap.maximum_live := 0; 
    
  let img = (OImages.rgb24 (OImages.load file [])) in
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
  (new_color, filepath); 
;; 

        



