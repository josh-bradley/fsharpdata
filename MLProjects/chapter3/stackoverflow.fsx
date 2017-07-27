#load "packages/FsLab/FsLab.fsx"

open FSharp.Data

type Questions = JsonProvider<"""https://api.stackexchange.com/2.2/questions?site=stackoverflow""">

let csQuestionsUrl = """https://api.stackexchange.com/2.2/questions?site=stackoverflow&tagged=C%23"""

Questions.Load(csQuestionsUrl).Items |> Seq.iter (fun q -> printfn "%s" q.Title)

let questionQuery = """https://api.stackexchange.com/2.2/questions?site=stackoverflow"""

let tagged tags query =
  tags 
    |> String.concat ";"
    |> sprintf "%s&tagged=%s" query 
let page p query = sprintf "%s&page=%i" query p
let pageSize pageSize query = sprintf "%s&pagesize=%i" query pageSize
let extractQuestions (query:string) = Questions.Load(query).Items

let ``F#`` = "F%23"
let ``C#`` = "C%23"

questionQuery 
  |> tagged (seq [``C#``; ``F#``; "javascript"])
  |> page 1
  |> pageSize 20
  |> extractQuestions

let fsSamples = questionQuery
                  |> tagged [``F#``]
                  |> extractQuestions

let csSamples = questionQuery
                  |> tagged [``C#``]
                  |> extractQuestions

let analyzeTags (qs:Questions.Item seq) = 
  qs 
  |> Seq.collect(fun questions -> questions.Tags)
  |> Seq.countBy id
  |> Seq.filter(fun (_,count) -> count > 2)
  |> Seq.sortBy(fun (_,count) -> -count)
  |> Seq.iter(fun (tag,count) -> printfn "%s,%i" tag count)

analyzeTags fsSamples
analyzeTags csSamples

