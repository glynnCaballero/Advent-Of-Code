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


module Day3 =
    let findMul (s: string) = 
        let mutable applyTuple = true
        s
        |> Seq.mapi (fun i el -> 
            if el = 'd' then
                let closureIndex = s.IndexOf(')', i) + 1
                let command = s.Substring(i, closureIndex - i)
                printfn "%s\n" command;
                if command = "don't()" || command = "do()" 
                then applyTuple <- command  = "do()"
                else () 
            else () // Soory

            let closureIndex = s.IndexOf(')', i)
            if el <> 'm' 
            then None
            else
                let candidateTuple = s.Substring(i,(closureIndex - i))
                let isValid = candidateTuple.Split(",")
                let validTuple = if candidateTuple.Contains("mul(") then candidateTuple.Replace("mul(", "") else ""  
                if Seq.length isValid = 2  && validTuple <> "" && applyTuple
                then
                    let isValid2 = fst (Int64.TryParse(validTuple.Split(",").[0])) && fst (Int64.TryParse ( validTuple.Split(",")[1])) 
                    let result() = validTuple.Split(",") |> Seq.map (int) |> Seq.reduce (fun x acc -> x * acc)
                
                    
                    if isValid2 then Some(validTuple,result()) else None
                else None                
        )
    let solve filePath =
        let input = File.ReadLines filePath
        let output = 
            input
            |> String.concat ""
            |> findMul
            |> (Seq.filter (Option.isSome)) 
            |> (Seq.map ( fun el -> el |> Option.get |> snd))
            |> (Seq.sum)




        printf "\ninput: %A \n" input
        output |> (printf "part1: %A \n")

module Day4 =
    let scan (grid: string List) (x,y) =
        let rows = grid.Length
        let columns = grid[0].Length

        let walk (startRow, startCol) (stepRow, stepCol) =
            let rec collectWords (r, c) acc =
                    if r >= 0 && r < rows && c >= 0 && c < columns && String.length acc <> 2 then
                        collectWords (r + stepRow, c + stepCol) (acc + (grid[r][c]).ToString())
                    else
                        acc
            collectWords (startRow, startCol) ""

        // let checkNeighbour (x1,y1) (stepRow, stepCol) = 
        //     let nextX,nextY = x1+stepRow, y1+stepCol
        //     if nextX >= 0 && nextX < rows && nextY >= 0 && nextY < columns then 
        //         Some(grid[x1+stepRow][y1])
        //     else None

        seq {
            // yield walk (x,y) (0,1)
            // yield walk (x,y) (0,-1)
            // yield walk (x,y) (1,0)
            // yield walk (x,y) (-1,0)
            yield walk (x,y) (1,1) // right down
            yield walk (x,y) (1,-1) // left down
            yield walk (x,y) (-1,-1) // left up
            yield walk (x,y) (-1,1) // right up
        }
        // |> Seq.filter (fun el -> el.Length > 0 && (el = "XMAS" || el = "SAMX"))
        
    let solve filePath =
        let input = File.ReadLines filePath |> Seq.toList
        
        let rows = input.Length
        let columns = input[0].Length

        let output = 
            seq {
                for row in 0 .. rows - 1 do
                    for col in 0 .. columns - 1 do
                        if input[row][col] = 'A' then yield scan input (row,col)
            }
            // |> Seq.concat
            // |> Seq.filter (fun el -> el = "XMAS" || el = "SAMX") // Part one
            // |> Seq.filter (fun el -> el |> Seq.forall (fun x -> x = "AS" || x = "AM") )
            |> Seq.filter (fun el -> el |> Seq.filter (fun x -> x = "AS") |> Seq.length = 2 &&  el |> Seq.filter (fun x -> x = "AM") |> Seq.length = 2)
            |> Seq.map (Seq.map (fun el -> el |> Seq.last)) // The two M's and S's must be adjacent. It's not a valid cross if MAM, or SAS; leaving the only valid cases as below
            |> Seq.filter (fun el -> 
                match el |> Seq.toList with
                | ['S';'S';'M';'M'] -> true
                | ['M';'S';'S';'M'] -> true
                | ['M';'M';'S';'S'] -> true
                | ['S';'M';'M';'S'] -> true
                | _ -> false
            )
            // |> Seq.concat

        printf "\ninput: %A \n" input
        output |>Seq.iter (printf "part1: %A \n")
        output |>Seq.length |> (printf "output: %A \n")