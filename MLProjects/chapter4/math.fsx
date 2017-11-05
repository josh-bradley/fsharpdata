#load "packages/FsLab/FsLab.fsx"
#r "packages/MathNet.Numerics.FSharp/lib/net40/MathNet.Numerics.FSharp.dll"
#r "packages/MathNet.Numerics/lib/net40/MathNet.Numerics.dll"
// Copy this into the main directory.
// #r "packages/MathNet.Numerics.MKL.Win/build/x64/MathNet.Numerics.MKL.dll"
#load "loaddata.fsx"

open FSharp.Charting
open FSharp.Data
open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

let A = vector [1.;2.;3.]
let B = matrix [
              [1.;2.]
              [3.;4.]
              [5.;6.]]

let C = A * A 
let D = A * B          
let E = A * B.Column(1)


type Data = CsvProvider<"../../data/day.csv">
let dataset = Data.Load("../../data/day.csv")
let data = dataset.Rows

type Vec = Vector<float>
type Mat = Matrix<float>

let cost (theata:Vec) (Y:Vec) (X:Mat) =
    let ps = Y - (theata * X.Transpose())
    ps * ps |> sqrt

//let predict (theta:Vec) (v:Vec) = theta * v
let X = matrix [for obs in data -> [1.; float obs.Instant]]
let Y = vector [for obs in data -> float obs.Cnt]

let theta = vector [6000.; -4.5]
//let result =  predict theta (X.Row(0))
cost theta Y X

let estimate (Y:Vec) (X:Mat) =
    (X.Transpose() * X).Inverse() * X.Transpose() * Y

estimate Y X


System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

open MathNet.Numerics
open MathNet.Numerics.Providers.LinearAlgebra.Mkl
Control.LinearAlgebraProvider <- MklLinearAlgebraProvider()

let seed = 314159
let rng = System.Random(seed)

let shuffle (arr:'a[]) = 
    let arr = arr |> Array.copy
    let length = arr.Length
    for i in (length - 1) .. -1 .. 1 do
      let temp = arr.[i]
      let j = rng.Next(0, i + 1)
      arr.[i] <- arr.[j]
      arr.[j] <- temp
    arr

let training,validation = 
    let shuffled = 
      data 
      |> Seq.toArray
      |> shuffle
    let size =
      0.7 * float (Array.length shuffled) |> int
    shuffled.[..size],
    shuffled.[size+1..]   

type Obs = Data.Row         
type Model = Obs -> float
type Featurizer = Obs -> float list

let predictor (f:Featurizer) (theata:Vec) =
    f >> vector >> (*) theata

let evaluate (model:Model) (data:Obs seq) =
    data 
    |> Seq.averageBy (fun obs -> 
        abs (model obs - float obs.Cnt))

let exampleFeaturizer (obs:Obs) =
    [1.; float obs.Instant]

let model (f:Featurizer) (data:Obs seq) =
    let Yt, Xt =
        data
        |> Seq.toList
        |> List.map (fun o -> float o.Cnt, f o)
        |> List.unzip
    let theta = estimate (vector Yt) (matrix Xt)
    let predict = predictor f theta 
    theta, predict

let (theata0,model0) = model exampleFeaturizer training

evaluate model0 training |> printfn "Training: %.0f"
evaluate model0 validation |> printfn "Training: %.0f"

let featurizer1 (obs:Obs) =
    [ 1.
      obs.Instant |> float
      obs.Atemp |> float
      obs.Hum |> float
      obs.Temp |> float
      obs.Windspeed |> float
    ]

let (theta1,model1) = model featurizer1 training

evaluate model1 training |> printfn "Training: %.0f"
evaluate model1 validation |> printfn "Training: %.0f"

let getWeekdayValue (weekdayId:int) dayOfWeek =
    match dayOfWeek = weekdayId with
    | true -> 1.
    | false -> 0.

getWeekdayValue 0 0

let featurizer2 (obs:Obs) =
    [ 1.
      obs.Instant |> float
      obs.Hum |> float
      obs.Temp |> float
      obs.Windspeed |> float
      getWeekdayValue 0 obs.Weekday
      getWeekdayValue 2 obs.Weekday
      getWeekdayValue 3 obs.Weekday
      getWeekdayValue 4 obs.Weekday
      getWeekdayValue 5 obs.Weekday
      getWeekdayValue 6 obs.Weekday
    ]

let (theta2,model2) = model featurizer2 training

evaluate model2 training |> printfn "Training: %.0f"
evaluate model2 validation |> printfn "Training: %.0f"


Chart.Combine [
  Chart.Line [for obs in data -> float obs.Cnt]
  Chart.Line [for obs in data -> model2 obs]
]

Chart.Point [ for obs in data -> float obs.Cnt, model1 obs]

let squareTempFeauturizer (obs:Obs) = 
    [ 1.
      obs.Temp |> float
      obs.Temp * obs.Temp |> float
    ]
 
let (_,squareTempModel) = model squareTempFeauturizer data


evaluate squareTempModel training |> printfn "Training: %.0f"
evaluate squareTempModel validation |> printfn "Training: %.0f"  

Chart.Combine [
  Chart.Point [ for obs in data -> obs.Temp, obs.Cnt]
  Chart.Point [ for obs in data -> obs.Temp, squareTempModel obs]
]  


let featurizer3 (obs:Obs) =
    [ 1.
      obs.Instant |> float
      obs.Hum |> float
      obs.Windspeed |> float
      getWeekdayValue 0 obs.Weekday
      getWeekdayValue 2 obs.Weekday
      getWeekdayValue 3 obs.Weekday
      getWeekdayValue 4 obs.Weekday
      getWeekdayValue 5 obs.Weekday
      getWeekdayValue 6 obs.Weekday
      obs.Temp |> float
      obs.Temp * obs.Temp |> float
    ]


let (theta3,model3) = model featurizer3 training

evaluate model3 training |> printfn "Training: %.0f"
evaluate model3 validation |> printfn "Training: %.0f"  