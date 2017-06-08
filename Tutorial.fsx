#load "packages/FsLab/FsLab.fsx"
open FSharp.Data

let wb = WorldBankData.GetDataContext()

wb.Countries.``Czech Republic``.CapitalCity
wb.Countries.``Czech Republic``.Indicators.``CO2 emissions (kt)``.[2010]

type Weather = JsonProvider<"http://api.openweathermap.org/data/2.5/forecast/daily?units=metric&q=Prague">



