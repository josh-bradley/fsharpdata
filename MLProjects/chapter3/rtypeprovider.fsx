#load "packages/FsLab/FsLab.fsx"

open FSharp.Data

let wb = WorldBankData.GetDataContext()
wb.Countries.Angola.CapitalCity

let countries = wb.Countries


let pop2000 = [for c in contries -> c.Indicators.``Population, total``.[2000]]
let pop2010 = [for c in contries -> c.Indicators.``Population, total``.[2010]]

open RProvider
open RProvider.``base``
open RProvider.graphics

let surface = [ for c in countries -> c.Indicators.``Surface area (sq. km)``.[2010]]
R.summary(surface) |> R.print

R.hist(surface |> R.log)

R.plot(surface, pop2010)

let pollution = [ for c in countries -> c.Indicators.``CO2 emissions (kt)``.[2000]]
let education = [ for c in countries -> c.Indicators.``School enrollment, primary and secondary (gross), gender parity index (GPI)``.[2000]]
let rdf = 
  [
    "Pop2000", box pop2000
    "Pop2010", box pop2010
    "Surface", box surface
    "Pollution", box pollution
    "Education", box education
  ] 
  |> namedParams
  |> R.data_frame

// Scatterplot of all features
rdf |> R.plot
rdf |> R.summary |> R.print

open Deedle

let series1 = series [ "Alpha",1.; "Bravo",2.; "Delta",4.]
let series2 = series [ "Bravo",20.; "Charlie",30.; "Delta",40.]
let toyFrame = frame [ "First", series1; "Second", series2]

// Get the sum and the mean from series or frame, deedle will automatically take missing values into account. 
series1 |> Stats.sum
toyFrame |> Stats.mean
// The ? in fsharp is used to access dynamic properties, in this case we don't know the names of the columns ahead of time to get a strogly type object.
toyFrame?First |> Stats.mean
toyFrame?Second |> Stats.mean
// Create a new column in the frame based of values from existing columns
toyFrame?AnyNameYouLike <- toyFrame?First * toyFrame?Second
toyFrame |> Stats.mean


