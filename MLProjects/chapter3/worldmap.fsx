#load "packages/FsLab/FsLab.fsx"

open FSharp.Data
open Deedle

let wb = WorldBankData.GetDataContext()
let countries = wb.Countries

let pop2000 = series [for c in countries -> c.Code, c.Indicators.``Population, total``.[2000]]
let pop2010 = series [for c in countries -> c.Code, c.Indicators.``Population, total``.[2010]]
let surface = series [for c in countries -> c.Code, c.Indicators.``Surface area (sq. km)``.[2010]]


let ddf = frame [
                "Pop2000", pop2000
                "Pop2010", pop2010
                "Surface", surface
                ]


ddf?Code <- ddf.RowKeys

open RProvider
open RProvider.``base``
open Deedle.RPlugin
open RProvider.rworldmap


let map = R.joinCountryData2Map(ddf, "ISO3", "Code")
R.mapCountryData(map, "Pop2000")

ddf?Density <- ddf?Pop2000 / ddf?Surface
R.mapCountryData(map, "Density")

ddf?Growth <- (ddf?Pop2010 - ddf?Pop2000) / ddf?Pop2000
let map1 = R.joinCountryData2Map(ddf, "ISO3", "Code")
R.mapCountryData(map1, "Growth")