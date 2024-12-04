namespace _2024
open System.IO
open System

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

module Day2 =
    let abs a = Math.Abs(int a)

    let isDescendingAscending someList =
        let rec checkIfDescendingAscending someList direction  = 
            match someList with
            | [] | [_] -> true
            | head :: adjacent :: tail -> 
                let currentDirection = head - adjacent
                let notBoth = 
                    match direction with 
                    | 0 -> true
                    | x-> if x > 0 then currentDirection > 0 else currentDirection < 0
                let distance = Math.Abs(int currentDirection) 
                if distance > 0 && distance <= 3 && notBoth
                then checkIfDescendingAscending (adjacent :: tail) currentDirection  else false

        checkIfDescendingAscending someList 0

    let hasApplicableSubset someList =
        let subsets = someList |> List.mapi(fun i _ -> someList |> List.indexed  |> List.filter (fun (j,_) -> i <> j ) |> List.map snd)
        subsets
        |> List.tryFind isDescendingAscending
        |> (<>) None
        
    let solve filePath =
        let input = 
            File.ReadLines filePath
            |> Seq.map (fun str -> str.Split(" ") |> Seq.map (int) |> Seq.toList) 
        let output = 
            input
            |> Seq.filter isDescendingAscending
            |> Seq.length

        let part2 =
            input 
            |> Seq.filter (fun els -> not (isDescendingAscending els))
            |> Seq.filter hasApplicableSubset
            |> Seq.length

        let someList = [1;9;2;3];
        hasApplicableSubset someList|> (printf "test: %A \n") 
        // System.Math.Abs -5


        printf "\ninput: %A \n" filePath
        output |> (printf "part1: %A \n")
        output + part2 |> (printf "part2: %A \n")