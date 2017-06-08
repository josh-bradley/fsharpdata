// #r "System.Xml.Linq.dll"
// #load "packages/FsLab/FsLab.fsx"
// open Deedle
// open FSharp.Data
// open XPlot.GoogleCharts
// open XPlot.GoogleCharts.Deedle

let data = [(0.0,1.0);(1.0,1.0);(10.0,1.0);(13.0,3.0);(4.0,10.0);(5.0,8.0)];

let distance (x1, y1) (x2, y2) : float =
 sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

let aggregate points : float * float =
  (List.averageBy fst points, List.averageBy snd points)
let clusterCount = 3
let centroids =
 let random = System.Random()
 [ for i in 1 .. clusterCount -> List.nth data (random.Next(data.Length)) ]

let closest centroids input =
 centroids
 |> List.mapi (fun i v -> i, v)
 |> List.minBy (fun (_, cent) -> distance cent input)
 |> fst

data |> List.map (fun point -> closest centroids point)

let rec update assignment =
       let centroids =
        [ for i in 0 .. clusterCount-1 ->
            let items =
             List.zip assignment data
             |> List.filter (fun (c, data) -> c = i)
             |> List.map snd
            aggregate items ]
       let next = List.map (closest centroids) data
       if next = assignment then assignment
       else update next
let assignment =
 update (List.map (closest centroids) data)