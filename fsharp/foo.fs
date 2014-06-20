module classifyDigits.Main

open System
open System.IO

type LabelPixels = { Label: int; Pixels: int[] }
let slurp_file file =
   File.ReadAllLines(file).[1..]
   |> Array.map (fun line -> line.Split(','))
   |> Array.map (fun numline -> Array.map (fun (x:string) -> Convert.ToInt32(x)) numline)
   |> Array.map (fun line -> { Label= line.[0]; Pixels=line.[1..] })

//load the trainingsample  
let trainingset = slurp_file("trainingsample.csv")

//  COMPUTING DISTANCES

// We need to compute the distance between images
// Math reminder: the euclidean distance is
// distance [ x1; y1; z1 ] [ x2; y2; z2 ] = 
// sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2))

let distance (p1: int[]) (p2: int[]) =
  Math.Sqrt (float(Array.sum (Array.map2 ( fun a b -> (pown (a-b) 2)) p1 p2) ))


//  WRITING THE CLASSIFIER FUNCTION

// We are now ready to write a classifier function!
// The classifier should take a set of pixels
// (an array of ints) as an input, search for the
// closest example in our sample, and predict
// the value of that closest element.

let classify (pixels: int[]) =
  fst (trainingset |> Array.map (fun x -> (x.Label, (distance pixels x.Pixels ) ))
                   |> Array.minBy (fun x -> snd x ))

// EVALUATING THE MODEL AGAINST VALIDATION DATA

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


let _ =
    Console.WriteLine("start...")
    let validationsample = slurp_file("validationsample.csv")
    let num_correct = (validationsample |> Array.map (fun p -> if (classify p.Pixels ) = p.Label then 1 else 0)
                                        |> Array.sum)
    Printf.printf "Percentage correct:%f\n" ((float(num_correct)/ (float(Array.length validationsample)))*100.0)
