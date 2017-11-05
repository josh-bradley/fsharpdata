#load "packages/FsLab/FsLab.fsx"
open FSharp.Data

let wb = WorldBankData.GetDataContext()

wb.Countries.``Czech Republic``.CapitalCity
wb.Countries.``Czech Republic``.Indicators.``CO2 emissions (kt)``.[2010]

type Weather = JsonProvider<"http://api.openweathermap.org/data/2.5/forecast/daily?units=metric&q=Prague">

module Person = 
    type T = {First:string; Last:string} with
        // member defined with type declaration
        member blur.FullName = 
            blur.First + " " + blur.Last

    // constructor
    let create first last = 
        {First=first; Last=last}
       
// test
let person = Person.create "John" "Doe"
let fullname = person.FullName