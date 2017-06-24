#r "System.IO"
let writeSubSetData start ende (sourceFile:string) (outputFile:string) = 
    using(new System.IO.StreamWriter(outputFile)) (fun output ->
    using(new System.IO.StreamReader(sourceFile)) (fun file -> 
        let check (idx, (s:string)) = idx < ende//5501
        let skip (idx, (s:string)) = idx < start//5001
        let head = file.ReadLine()
        output.WriteLine(head)
        Seq.initInfinite (fun x -> (x, file.ReadLine()))
           |> Seq.skipWhile skip
           |> Seq.takeWhile check
           |> Seq.iter (fun (_,s) -> output.WriteLine(s))
        )
    )

writeSubSetData 1 5000 "./MLProjects/chapter1/data/train.csv" "./MLProjects/chapter1/data/trainsubset.csv"
writeSubSetData 5001 5501 "./MLProjects/chapter1/data/train.csv" "./MLProjects/chapter1/data/validate.csv"











