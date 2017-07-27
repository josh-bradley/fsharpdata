#load "packages/FsLab/FsLab.fsx"

open FSharp.Data

let wb = WorldBankData.GetDataContext()
wb.Countries.Angola.CapitalCity

let contries = wb.Countries
let pop2000 = [for c in contries -> c.Indicators.``Population, total``.[2000]]
let pop2010 = [for c in contries -> c.Indicators.``Population, total``.[2010]]

