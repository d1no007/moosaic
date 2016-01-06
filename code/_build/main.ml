open Pixels 
open PointTree 

let () = 
    (* ensure proper usage *) 
    if Array.length Sys.argv <> 3 then 
        Printf.printf "Please enter a minimum tile number and a picture!" 
    (* finds new tile size *)  
    else let num, img = Sys.argv.(1), Sys.argv.(2) in 
        let size = gridder num img in
        crop_resize_all size; 
        let tree = build_balanced (avg_color_images) 0 empty in 
        blitz (tree) (ref "out"^img) (size)     
;;
        
