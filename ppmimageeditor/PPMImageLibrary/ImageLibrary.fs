// Jose E. Rodriguez
// CS 341 Spring 2018
// Project #6 - Image Processing
// University of Illinois at Chicago

module PPMImageLibrary

open System.Security

#light


//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage (image:(int*int*int) list list) = 
  match image with
  | [] -> printfn "**END**"
  | hd::tl -> printfn "%A" hd
              OutputImage tl
           
let DebugOutput(width:int, height:int, depth:int, image:(int*int*int) list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage image


//
// TransformFirstThreeRows:
//
// An example transformation: replaces the first 3 rows of the given image
// with a row of Red, White and Blue pixels (go USA :-).
//
let rec BuildRowOfThisColor row color = 
  match row with
  | []     -> []
  | hd::tl -> color :: BuildRowOfThisColor tl color

let TransformFirstThreeRows(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let row1 = List.head image
  let row2 = List.head (List.tail image)
  let row3 = List.head (List.tail (List.tail image))
  let tail = List.tail (List.tail (List.tail image))
  let newRow1 = BuildRowOfThisColor row1 (255,0,0)      // red:
  let newRow2 = BuildRowOfThisColor row2 (255,255,255)  // white:
  let newRow3 = BuildRowOfThisColor row3 (0,0,255)      // blue:
  let newImage = newRow1 :: newRow2 :: newRow3 :: tail
  newImage


//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let Flatten (SL:string list) = 
  List.reduce (fun s1 s2 -> s1 + " " + s2) SL

let Image2ListOfStrings (image:(int*int*int) list list) = 
  List.map (fun TL -> List.map (fun (r,g,b) -> r.ToString()+" "+g.ToString()+" "+b.ToString()+" ") TL) image
  |> List.map Flatten

let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let L = [ "P3" ] @ 
          [ System.Convert.ToString(width); System.Convert.ToString(height) ] @
          [ System.Convert.ToString(depth) ] @
          (Image2ListOfStrings image)
  System.IO.File.WriteAllLines(filepath, L)
  true  // success



//
// Grayscale:
//
// Converts the image into grayscale and returns the resulting image as a list of lists. 
// Conversion to grayscale is done by averaging the RGB values for a pixel, and then 
// replacing them all by that average. So if the RGB values were 25 75 250, the average 
// would be 116, and then all three RGB values would become 116 — i.e. 116 116 116.
//
let rec Grayscale (width:int, height:int, depth:int, image:(int*int*int) list list) = 
  
  // Grayscale the pixel
  let GS_Pixel pixel:(int*int*int) =
    let r,g,b = pixel
    let avg   = (r+g+b)/3
    (avg,avg,avg)

  // Grayscale the row
  let GS_Row row:(int*int*int) list =
    List.map (GS_Pixel) row
  
  // Grayscale every row in the image
  let GS_Image =
    List.map (GS_Row) image
  
  // Return the Grayscaled image
  GS_Image


//
// Threshold:
//
// Thresholding increases image separation --- dark values become darker and light values
// become lighter.  Given a threshold value in the range 0 < threshold < MaxColorDepth,
// all RGB values > threshold become the max color depth (white) while all RGB values
// <= threshold become 0 (black).  The resulting image is returned.
//
let rec Threshold(width:int, height:int, depth:int, image:(int*int*int) list list, threshold:int) = 
  
  // Threshold every individual pixel
  let TH_Pixel pixel:(int*int*int) =
    let r,g,b = pixel
    let r_val = if r <= threshold then 0 else 255
    let g_val = if g <= threshold then 0 else 255
    let b_val = if b <= threshold then 0 else 255
    (r_val, g_val, b_val)

  // Threshold every row
  let TH_Row row:(int*int*int) list =
    List.map (TH_Pixel) row

  // Threshold every row in the image
  let TH_Image =
    List.map (TH_Row) image
  
  // Return the image after going through the Threshold
  TH_Image



//
// FlipHorizontal:
//
// Flips an image so that what’s on the left is now on the right, and what’s on 
// the right is now on the left. That is, the pixel that is on the far left end
// of the row ends up on the far right of the row, and the pixel on the far right 
// ends up on the far left. This is repeated as you move inwards toward the center 
// of the row. 
//
let rec FlipHorizontal(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  
  // Reverse the order of the pixels in the row
  let Reverse_Row row:(int*int*int) list =
    List.rev row
  
  // Reverse the order of the rows in the list
  let Flipped_Image =
    List.map (Reverse_Row) image

  // Return the Horizontally Flipped image
  Flipped_Image



//
// Zoom:
//
// Zooms the image by the given zoom factor, which is an integer 0 < factor < 5.  
// The function uses the nearest neighbor approach where each pixel P in the original 
// image is replaced by a factor*factor block of P pixels.  For example, if the zoom 
// factor is 4, then each pixel is replaced by a 4x4 block of 16 identical pixels. 
// The nearest neighbor algorithm is the simplest zoom algorithm, but results in 
// jagged images.  The resulting image is returned.
//
let rec Zoom(width:int, height:int, depth:int, image:(int*int*int) list list, factor:int) = 
  
  // Duplicate each element by the factor
  let rec _Duplicate elem factorVal =
    match factorVal with
    | 0 -> []
    | _ -> elem::(_Duplicate elem (factorVal-1))

  // Duplicate each elem in the list
  let rec Duplicate L =
    match L with
    | []     -> []
    | hd::tl -> (_Duplicate hd factor) @ (Duplicate tl)

  // Duplicate each pixel in each row horizontally
  let VertFactoredImage = List.map (Duplicate) image

  // Duplicate each row in the image vertically
  Duplicate VertFactoredImage



//
// RotateRight90:
//
// Rotates the image to the right 90 degrees.
//
let rec RotateRight90(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  
  // Get head of list, return head
  let getHeadOfList L:(int*int*int) =
    match L with
    | []     -> (0,0,0)
    | hd::tl -> hd

  // Transform every list into the tail of the list
  let getTailOfList L =
    match L with
    | []     -> []
    | hd::tl -> tl

  // Get the first element of every row (a column) and make that a new row in the new image
  let rec _columnToRow (L:(int*int*int) list list, newList:(int*int*int) list list, width:int) =
    match width with
    | 0 -> newList
    | _ -> let newRow = List.rev (List.map (getHeadOfList) L)
           let newList2 = List.map (getTailOfList) L
           (_columnToRow( newList2, (newRow::newList), (width-1) ) )

  // Helper function for above function
  let columnToRow L width = 
   List.rev (_columnToRow(image, [], width))
  
  // Call the helper function and return rotated image
  (columnToRow image width)
