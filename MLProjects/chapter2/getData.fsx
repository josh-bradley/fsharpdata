open System.IO

let fileName = "stop-word-list.csv"
let path = __SOURCE_DIRECTORY__ + @"/data/" + fileName

let stopWords = File.ReadLines path
                |> List.ofSeq
                |> List.head
                |> (fun x -> x.Split(','))
                |> Array.map (fun word -> word.Trim())


let add2 x = (x + 2).ToString()
let add3 str = printf "%s" str

let add5 = add2 >> add3






