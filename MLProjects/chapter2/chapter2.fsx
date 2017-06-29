#load @"NaiveBayes.fsx"

open NaiveBayes

open System.IO

type DocType = Ham | Spam

let parseDocType = function 
      | "ham" -> Ham
      | "spam" -> Spam
      | _ -> failwith "Unknown label"

let parseLine (line:string) =
    let split = line.Split('\t')
    let label = split.[0] |> parseDocType
    let message = split.[1]
    (label, message)

let fileName = "SMSSpamCollection"
let path = __SOURCE_DIRECTORY__ + @"..\data\" + fileName

let dataset = File.ReadLines path
                |> List.ofSeq
                |> List.map parseLine


open System.Text.RegularExpressions

let matchWords = Regex(@"\w+")

let tokens (text:string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let training = dataset
                |> List.take 4574
                |> Array.ofList

let txtClassifier = train training tokens (["txt"] |> set)              

let validation = dataset
                  |> List.skip 4574
                  |> Array.ofList


validation
|> Seq.averageBy (fun (docType,sms) -> 
                    match (txtClassifier sms) = docType with
                    | true ->  1.0
                    | false -> 0. )
|> printfn "Based no 'txt', correctly classified:%.3f"                