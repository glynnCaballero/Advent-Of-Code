namespace _2024
open System.IO

module Day1 =
    let abs a = sqrt (a*a)
    let solve filePath =
        let input = File.ReadLines filePath
        let output = input

        printf "\ninput: %A \n" filePath
        printf "output: %A \n" output

        input 
        |> Seq.map (fun str -> str.Split(" "))
        |> Seq.map (Seq.filter (fun str -> str <> ""))
        |> Seq.map (Seq.map (int))
        |> Seq.fold (fun arr el -> 
            let (left,right) = arr
            match Seq.toList el with
            | [a;b] -> (a :: left, b :: right)
            | _ -> arr
            // arr
        ) ([],[])
        |> (fun (left,right) -> (List.sort left, List.sort right))
        // |> (fun (left,right) -> List.zip left right) // part 1
        // |> List.fold (fun acc (l,r) -> acc + abs(float (l - r)) ) 0.0
        |> (fun (left,right) -> 
            left 
            |> List.fold (fun acc a -> 
                let countInRight = right |> List.filter (fun b -> a = b) |> List.length
                acc + (a*countInRight)
            ) 0 
        )
        |> (printf "\nel: %A \n")