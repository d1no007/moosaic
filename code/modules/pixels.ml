open Camlimages;;
open Images;;
open OImages;;
open Info;;
open Color;;
open PointTree;;

(* The Pixels module, in which we do all our pixel and image 
 * manipulation *)
module type PIXELS = 
    sig 
      (* finds the average color of an image and returns it and the 
       * image itself  *)
      val average_color : bytes ref -> rgb24 option -> point * bytes ref
      
      (* finds the avg color of all the images in the library and 
       * returns a list of the average colors and the images *)
      val avg_color_images : (point * bytes ref) list 
      
      (* crops and resizes and image *) 
      val crop_resize : bytes ref -> int -> unit 
      
      (* crops and resizes all the images in the library to the 
       * designated size indicated by the user -- we realize 
       * that this is inefficient, but seeing as it was impossible
       * to manipulate the image once it was in the tree, it made 
       * the most sense, especially since we only had around 300
       * images in the library, and most of the time we use 
       * way more than 300 images in a picture *)
      val crop_resize_all : int -> unit 
      
      (* divides up the image into tiles based on the users input
       * for minimum number of tiles in a moosaic. returns the 
       * dimmensions that each tile needs to be *) 
      val gridder : int -> bytes ref -> int 
      
      (* swaps in every tile in the tree based on the average
       * color *) 
      val blitz : PointTree.tree -> bytes ref -> int -> unit 
    end


module Pixels : PIXELS = 
    struct 
    
    (* Iterates through an rgb24 bitmap image and calculates the average
     * color of the image by summing the red, green, and blue of each
     * pixel respectively, and then calculating the average RGB.
     * Returns the a tuple of the avg, color and filepath. In the case
     * that a tile is used, the file path will be empty *)
    let average_color (filepath : string ref) (tile : rgb24 option) :
    (Rgb.t * string ref) = 
      (* Used for "swapping" memory from heap if there is overflow. 
       * Integrated into camlimages library, but buggy so it is disabled. *)
      Bitmap.maximum_live := 0;
      let img = 
      (match tile with 
       | None -> let file = !filepath in (OImages.rgb24 (OImages.load file []))
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

    (* Calculates the average color of all the images in the "outphotos" folder
     * These are the images that will be used as tiles for the photomosaic. *)
    let avg_color_images = 
      let info = Array.to_list (Sys.readdir "outphotos") in
      let color_path = List.map (fun a -> 
        (average_color (ref ("outphotos/" ^ a)) None)) info in
       color_path
    ;;

    (* Crops a image to a square by cropping evenly from both sides, and 
     * then resizes it to the inputted dimensions (s). Does this to
     * maintain aspect ratio for square monosized tiles. *)
    let crop_resize (filepath : string ref) (s : int) : unit =
      Bitmap.maximum_live := 0; 
      let file = !filepath in 
      let outfile = "out" ^ file in
      let fmt, _ = Images.file_format file in
      let img = OImages.load file [] in
      if img#image_class <> ClassRgb24 then 
        Sys.remove file
      else 
     (let img = OImages.rgb24 img in 
        (* Helper function to crop image evenly from both sides, whether 
         * it be cropping height or width. *)
        let square_crop i (w : int) (h : int) = 
          let diff = abs (w - h) in
            if w > h then i#sub (diff / 2) 0 (w - diff) h
            else i#sub 0 (diff / 2) w (h - diff) in
      let cropped = square_crop img img#width img#height in
      let new_img = cropped#resize None s s in
        new_img#save outfile (Some fmt) [Save_Quality 95])
    ;;

    (* Crops and resizes all images in the "photos" folder into monosized 
     * tiles of the given dimensions (s). Place the copies into an "outphotos"
     * folder. *)
    let crop_resize_all (s : int) = 
      let paths = Array.to_list (Sys.readdir "photos") in
        List.iter (fun x -> crop_resize (ref ("photos/" ^ x)) s) paths
    ;;

    (* Takes in the minimum number of tiles the user wants and the base image,
     * and returns the tile dimensions (square). Also crops the base image
     * to perfectly fit the calculated tile dimensions. The tile dimensions 
     * are calculated from an algorithm the uses sqrt n for the base of its 
     * calculations. n being the minimum number of tiles the user wants. *)
    let gridder (n : int) (filepath : string ref) : int = 
      Bitmap.maximum_live := 0; 
      let file = !filepath in 
      let outfile = "out" ^ file in
      let fmt, _ = Images.file_format file in
      let img = OImages.rgb24 (OImages.load file []) in
        let small_side = if img#width > img#height then img#height 
                         else img#width in
        let tile_side = truncate ((float small_side) /. (sqrt (float n))) in 
        let new_width = tile_side * (img#width / tile_side) in
        let new_height = tile_side * (img#height / tile_side) in  
        let lost_pixels_width = img#width - new_width in
        let lost_pixels_height = img#height - new_height in
        let bmp_cropped = img#sub (lost_pixels_width / 2) 
          (lost_pixels_height / 2) (img#width - lost_pixels_width) 
          (img#height - lost_pixels_height) in
            bmp_cropped#save outfile (Some fmt) [Save_Quality 95] ; 
            tile_side
    ;;

    (* Selects an image stored in the kd-tree using the nearest neighbor search 
     * to find the image with the nearest avg RGB to the current tile, bitblits
     * the image onto the tile, and moves on. Does this for every tile in the 
     * base image. *)
    let blitz (img_tree : PointTree.tree) (base_path : string ref) (s : int) : unit = 
      Bitmap.maximum_live := 0; 
      let file = !base_path in 
      let outfile = "out" ^ file in
      let fmt, _ = Images.file_format file in
      let img = OImages.rgb24 (OImages.load file []) in
      (* Does everything stated above *)
        let rec swapper s img_tree x y =
          let select_image (t: PointTree.tree) (elt: (PointTree.elt)) = 
            let (_, path) = PointTree.nearest_neighbor elt t in 
            let file = !path in
            let img = (OImages.rgb24 (OImages.load file [])) in img in
          if x <> img#width then 
          let lit = select_image img_tree (average_color (ref "") (Some 
            (img#sub x y s s))) in
              (lit#blit 0 0 img x y s s; swapper s img_tree (x + s) y)
          else if y <> (img#height - s) then swapper s img_tree 0 (y + s)
          else () in
        swapper s img_tree 0 0 ;
        img#save outfile (Some fmt) [Save_Quality 95]  
    ;;  
end

