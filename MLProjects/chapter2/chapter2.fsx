#load @"NaiveBayes.fsx"
#load @"getData.fsx"

open NaiveBayes
open GetData
open System.IO

type DocType = Ham | Spam

// Parse string label for each line to either ham or spam
let parseDocType = function 
      | "ham" -> Ham
      | "spam" -> Spam
      | _ -> failwith "Unknown label"

// Split line into its label and the text of the message
let parseLine (line:string) =
    let split = line.Split('\t')
    let label = split.[0] |> parseDocType
    let message = split.[1]
    (label, message)

let fileName = "SMSSpamCollection"
let path = __SOURCE_DIRECTORY__ + @"/data/" + fileName

let dataset = File.ReadLines path
                |> List.ofSeq
                |> List.map parseLine


open System.Text.RegularExpressions

let matchWords = Regex(@"\w+")

// Get a set of tokens (words in this case) from a string
let wordTokenizer (text:string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let training = dataset
                |> List.skip 1000
                |> Array.ofList


let validation = dataset
                  |> List.take 1000
                  |> Array.ofList            

// Get the entire set of tokens across a sequence of messages
let vocabulary (tokenizer:Tokenizer) (corpus:string seq) = 
    corpus
    |> Seq.map tokenizer
    |> Set.unionMany

// All tokens in the training set.
let allTokens =
    training
    |> Seq.map snd
    |> vocabulary wordTokenizer

let fullClassifier = train training wordTokenizer allTokens

let txtClassifier = train training wordTokenizer (["txt"] |> set)              


let evaluate (tokenizer:Tokenizer) (tokens:Token Set) = 
    let classifier = train training tokenizer tokens

    validation
    |> Seq.averageBy (fun (docType,sms) -> 
                        match (classifier sms) = docType with
                        | true ->  1.0
                        | false -> 0. )
    |> printfn "Correctly classified:%.3f"                

evaluate wordTokenizer allTokens


let caseTokenizer (text:string) =
    text
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let casedTokens =
    training
    |> Seq.map snd
    |> vocabulary caseTokenizer

evaluate caseTokenizer casedTokens

let top n (tokenizer:Tokenizer) (docs:string []) =
    let tokenized = docs |> Array.map tokenizer
    let allTokens = tokenized |> Set.unionMany
    allTokens 
    |> Seq.sortBy (fun t -> - countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let ham,spam = 
    let rawHam,rawSpam = 
        training
        |> Array.partition (fun (lbl,_) -> lbl = Ham)
    rawHam |> Array.map snd,
    rawSpam |> Array.map snd

let countUniqueTokens (docs: string seq) =
    docs |> vocabulary caseTokenizer |> Set.count

let getTopPercentageOfTokens (percentage:int) count (docs:string []) =
    let n = (float count) * (float percentage) / 100.
    top (int n) caseTokenizer docs
let hamCount = ham |> countUniqueTokens
let spamCount = spam |> countUniqueTokens

let topHamTokens = ham |> getTopPercentageOfTokens 1 hamCount
let topSpamTokens = spam |> getTopPercentageOfTokens 1 spamCount

let topTokens = Set.union topHamTokens topSpamTokens

evaluate caseTokenizer topTokens

let specificTokens = Set.intersect topHamTokens topSpamTokens
                        |> Set.difference topTokens
                        //|> evaluate caseTokenizer                                                

let phoneRegex = Regex(@"0[7-9]\d{9}")
let txtRegex = Regex(@"\b\d{5}\b")
let (|PhoneNo|TextNo|Other|) str = 
    if phoneRegex.Match(str).Length > 0 then 
        PhoneNo
    else if txtRegex.Match(str).Length > 0 then
        TextNo
    else 
        Other

let mapSpecialTokens str =
    match str with
    | PhoneNo -> "__PHONE__"    
    | TextNo -> "__TEXT__"    
    | Other -> str    

let smartTokenizer = caseTokenizer >> Set.map mapSpecialTokens

let smartTokens = specificTokens |> Set.add "__PHONE__" |> Set.add "__TEXT__"

evaluate smartTokenizer smartTokens

let lengthAnalysis len = 
    let long (msg:string) = msg.Length > len

    let ham, spam = 
        dataset
        |> List.partition (fun (docType,_) -> docType = Ham)

    let spamAndLongCount = 
        spam 
        |> List.filter (fun (_,sms) -> long sms)
        |> List.length

    let longCount = 
        dataset 
        |> List.filter (fun (_,sms) -> long sms)
        |> List.length

    let pSpam = (float spam.Length) / (float dataset.Length)
    let pLongIfSpam = float spamAndLongCount / float spam.Length
    let pLong = float longCount / float dataset.Length

    let pSpamIfLong = pLongIfSpam * pSpam / pLong
    pSpamIfLong

for l in 10 ..10 .. 130 do 
    printfn "P(Spam if Length > % i) = %.4f" l (lengthAnalysis l)


let bestClassifier = train training smartTokenizer smartTokens
let byDocType docType docTypeName =
    validation
    |> Seq.filter(fun(dt,_) -> dt = docType)
    |> Seq.averageBy (fun (docType,sms) -> 
        if docType = bestClassifier sms
        then 1.0
        else 0.)
    |> printfn "Properly classified %s: %.5f" docTypeName

byDocType Ham "Ham"
byDocType Spam "Spam"


    