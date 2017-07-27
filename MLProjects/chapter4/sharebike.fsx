#load "packages/FsLab/FsLab.fsx"

open FSharp.Data
open FSharp.Charting
let path = __SOURCE_DIRECTORY__ + @"/data/day.csv"

type Data = CsvProvider<"../../data/day.csv">
let dataset = Data.Load("../../data/day.csv")
let data = dataset.Rows


let all = Chart.Line [for obs in data -> obs.Cnt]

let movingAverage (n: int) (series: float seq) =
    series    
      |> Seq.windowed n
      |> Seq.map (fun xs -> xs |> Seq.average)
      |> Seq.toList

let count = seq [for obs in data -> (float obs.Cnt)] 


Chart.Combine [
  Chart.Line (count |> List.ofSeq)
  Chart.Line (movingAverage 7 count)
  Chart.Line (movingAverage 30 count)
]    

let baseline = 
    let avg = data |> Seq.averageBy (fun x -> float x.Cnt)
    data |> Seq.averageBy (fun x -> abs(float x.Cnt - avg))

type Obs = Data.Row

// Function to generate models based on the data from observations. 
// Each theata is a modification for a specific observation feature
let model (theta0, theata1) (obs: Obs) =
    theta0 + theata1 * (float obs.Instant)

let model0 = model (4505., 0.)
let model1 = model (6000., -4.5)

Chart.Combine [
  Chart.Line count
  Chart.Line [for obs in data -> model0 obs]
  Chart.Line [for obs in data -> model1 obs]
]

type Model = Obs -> float

let cost (data:Obs seq) (m:Model) =
    data 
    |> Seq.sumBy (fun x -> pown (float x.Cnt - m x) 2)
    |> sqrt

let overallCost = cost data  
overallCost model0 |> printfn "Cost of model0: %.0f"
overallCost model1 |> printfn "Cost of model1: %.0f"






  

