type Observation = { Label: string; Pixels: int list }

let toObservation (_,(line:string)) =
    let cols = line.Split(',')
    let label = cols.[0]
    let pixels = cols.[1..] 
                    |> Array.toList 
                    |> List.map int
    {Label = label; Pixels = pixels}


let getObservations (filePath:string) = 
    using(new System.IO.StreamReader(filePath)) (fun file -> 
        let check (idx, (s:string)) = s <> null
        Seq.initInfinite (fun x -> (x, file.ReadLine()))
           |> Seq.takeWhile check
           |> Seq.toList
           |> (fun s -> s.[1..])
           |> List.map toObservation
)

let distanceManhatten (pixels1:int list) (pixels2:int list) = 
    pixels1 
        |> List.zip pixels2
        |> List.sumBy (fun (x,y) -> abs(x-y))

let  distanceEclidean (pixels1:int list) (pixels2:int list) = 
      pixels1
        |> List.zip pixels2
        |> List.sumBy (fun (x,y) -> pown (x-y) 2)

let predict (observations:Observation list) (pixels:int list) distance =
    observations
        |> List.minBy (fun { Pixels=p } -> distance p pixels)        
        |> fun x -> x.Label


let validateData validationData trainingData distance = 
    validationData
    |> List.averageBy (fun { Label = expected; Pixels = validatePixels} ->  if expected = (predict trainingData validatePixels distance) then 1. else 0.)

let trainingData = getObservations "./MLProjects/chapter1/data/trainsubset.csv"
let validationData = getObservations "./MLProjects/chapter1/data/validate.csv"

let ecideanResult = validateData validationData trainingData distanceEclidean
let manhattenResult = validateData validationData trainingData distanceManhatten

let printResult methodName result =
    printfn "The %s method got %f correct" methodName result

printResult "Ecidean" ecideanResult
printResult "Manhatten" manhattenResult

    
    