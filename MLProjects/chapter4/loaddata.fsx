#load "packages/FsLab/FsLab.fsx"

open FSharp.Data
open FSharp.Charting
let path = __SOURCE_DIRECTORY__ + @"/data/day.csv"

type Data = CsvProvider<"../../data/day.csv">
let dataset = Data.Load("../../data/day.csv")
let data = dataset.Rows
