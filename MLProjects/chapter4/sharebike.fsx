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
let model1 = model (4505., 2.)

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

let gradientDecent theata0 theata1 feature1 y theatak featurek alpha  = 
    theatak - 2. * alpha * featurek * (theata0 + theata1 * feature1 - y)

let update alpha (theata0, theata1) (obs:Obs) =
    let y = float obs.Cnt
    let x = float obs.Instant
    let gd = gradientDecent theata0 theata1 x y 
    let theata0' = gd theata0 1. alpha
    let theata1' = gd theata1 x alpha
    theata0', theata1'

let obs100 = data |> Seq.nth 100
let testUpdate = update 0.00001 (0.,0.) obs100
cost [obs100] (model (0.,0.))
cost [obs100] (model testUpdate)

let stochastic alpha (theata0, theata1) =
    data 
    |> Seq.fold (fun (t0, t1) obs -> 
        printfn "%.4f,%.4f" t0 t1
        update alpha (t0,t1) obs) (theata0, theata1)

let tunealpha = 
    [ for r in 1..20 -> 
       (pown 0.1 r), stochastic (pown 0.1 r) (0.,0.) |> model |> overallCost]

tunealpha 
  |> List.iter(fun (a,cost) -> 
                printfn "%.20f\t%.4f" a cost)

let alpha = pown 0.1 8
let model2 = model (stochastic alpha (0.0,0.0))
Chart.Combine [
  Chart.Line count
  Chart.Line [for obs in data -> model2 obs]
]

let hiRate = alpha * 10.
let errorEval = 
    data
    |> Seq.scan (fun (t0, t1) obs -> update hiRate (t0,t1) obs) (0., 0.)
    |> Seq.map (model >> overallCost)
    |> Chart.Line

let batchUpdate alpha (theata0, theata1) (data:Obs seq) =
    let updates =
        data
        |> Seq.map (update alpha (theata0, theata1))
    let theata0' = updates |> Seq.averageBy fst      
    let theata1' = updates |> Seq.averageBy snd
    theata0', theata1'

let batch alpha iters = 
    let rec search (t0, t1) i =
        match i with
        | 0 -> (t0, t1)
        | _ -> search(batchUpdate alpha (t0,t1) data) (i-1)
    search (0.,0.) iters      

let batchedError alpha =
    Seq.unfold (fun(t0, t1) -> 
                let (t0', t1') = batchUpdate alpha (t0, t1) data
                let error = model (t0,t1) |> overallCost
                Some(error, (t0',t1'))) (0.,0.) 
    |> Seq.take 100
    |> Seq.toList
    |> Chart.Line 

batchedError 0.0000001           

batch 0.000001 1000

