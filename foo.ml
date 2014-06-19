(* OCaml version submitted by @camlspotter
   compile with:
    ocamlopt str.cmxa -o classifyDigitsArray classifyDigitsArray.ml 
*)

(*
// This F# dojo is directly inspired by the 
// Digit Recognizer competition from Kaggle.com:
// http://www.kaggle.com/c/digit-recognizer
// The datasets below are simply shorter versions of
// the training dataset from Kaggle.
 
// The goal of the dojo will be to
// create a classifier that uses training data
// to recognize hand-written digits, and
// evaluate the quality of our classifier
// by looking at predictions on the validation data.
 
*)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

(*
 
// Two data files are included in the same place you
// found this script: 
// trainingsample.csv, a file that contains 5,000 examples, and 
// validationsample.csv, a file that contains 500 examples.
// The first file will be used to train your model, and the
// second one to validate the quality of the model.
 
// 1. GETTING SOME DATA
 
// First let's read the contents of "trainingsample.csv"
 
*)

type labelPixels = { label: int; pixels: int array }
let slurp_file file =
  List.tl (read_lines file)
  |> List.map (fun line -> Str.split (Str.regexp ",") line )
  |> List.map (fun numline -> List.map (fun (x:string) -> int_of_string x) numline)
  |> List.map (fun line ->
    { label= List.hd line;
      pixels= Array.of_list @@ List.tl line })
  |> Array.of_list

let trainingset = slurp_file "./trainingsample.csv"

(* 
// 6. COMPUTING DISTANCES
 
// We need to compute the distance between images
// Math reminder: the euclidean distance is
// distance [ x1; y1; z1 ] [ x2; y2; z2 ] = 
// sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2))
*)

let array_fold_left2 f acc a1 a2 =
  let open Array in
  let len = length a1 in
  let rec iter acc i =
    if i = len then acc
    else
      let v1 = unsafe_get a1 i in
      let v2 = unsafe_get a2 i in
      iter (f acc v1 v2) (i+1)
  in
  iter acc 0

let distance p1 p2 =
  sqrt
  @@ float_of_int
  @@ array_fold_left2 (fun acc a b -> let d = a - b in acc + d * d) 0 p1 p2

(* 
// 7. WRITING THE CLASSIFIER FUNCTION
 
// We are now ready to write a classifier function!
// The classifier should take a set of pixels
// (an array of ints) as an input, search for the
// closest example in our sample, and predict
// the value of that closest element.
 
*)

let classify (pixels: int array) =
  fst (
    Array.fold_left (fun ((min_label, min_dist) as min) (x : labelPixels) ->
      let dist = distance pixels x.pixels in
      if dist < min_dist then (x.label, dist) else min)
      (max_int, max_float) (* a tiny hack *)
      trainingset
  )

(*
// 8. EVALUATING THE MODEL AGAINST VALIDATION DATA
 
// Now that we have a classifier, we need to check
// how good it is. 
// This is where the 2nd file, validationsample.csv,
// comes in handy. 
// For each Example in the 2nd file,
// we know what the true Label is, so we can compare
// that value with what the classifier says.
// You could now check for each 500 example in that file
// whether your classifier returns the correct answer,
// and compute the % correctly predicted.
*)

let validationsample = slurp_file "./validationsample.csv"

let num_correct =
  Array.fold_left (fun sum p -> sum + if classify p.pixels = p.label then 1 else 0) 0 validationsample

let _ =
  Printf.printf "Percentage correct:%f\n"
  @@ float_of_int num_correct /. float_of_int (Array.length validationsample) *.100.0
