namespace Solutions._2024
open System.IO

open System.IO
open System

module Day1 =
    let abs a = sqrt (a * a)

    let solve filePath =
        let input = File.ReadLines filePath
        let output = input

        printf "\ninput: %A \n" filePath
        printf "output: %A \n" output

        input
        |> Seq.map (fun str -> str.Split(" "))
        |> Seq.map (Seq.filter (fun str -> str <> ""))
        |> Seq.map (Seq.map (int))
        |> Seq.fold
            (fun arr el ->
                let (left, right) = arr

                match Seq.toList el with
                | [ a; b ] -> (a :: left, b :: right)
                | _ -> arr
            // arr
            )
            ([], [])
        |> (fun (left, right) -> (List.sort left, List.sort right))
        // |> (fun (left,right) -> List.zip left right) // part 1
        // |> List.fold (fun acc (l,r) -> acc + abs(float (l - r)) ) 0.0
        |> (fun (left, right) ->
            left
            |> List.fold
                (fun acc a ->
                    let countInRight = right |> List.filter (fun b -> a = b) |> List.length
                    acc + (a * countInRight))
                0)
        |> (printf "\nel: %A \n")

module Day2 =
    let abs a = Math.Abs(int a)

    let isDescendingAscending someList =
        let rec checkIfDescendingAscending someList direction =
            match someList with
            | []
            | [ _ ] -> true
            | head :: adjacent :: tail ->
                let currentDirection = head - adjacent

                let notBoth =
                    match direction with
                    | 0 -> true
                    | x -> if x > 0 then currentDirection > 0 else currentDirection < 0

                let distance = Math.Abs(int currentDirection)

                if distance > 0 && distance <= 3 && notBoth then
                    checkIfDescendingAscending (adjacent :: tail) currentDirection
                else
                    false

        checkIfDescendingAscending someList 0

    let hasApplicableSubset someList =
        let subsets =
            someList
            |> List.mapi (fun i _ -> someList |> List.indexed |> List.filter (fun (j, _) -> i <> j) |> List.map snd)

        subsets |> List.tryFind isDescendingAscending |> (<>) None

    let solve filePath =
        let input =
            File.ReadLines filePath
            |> Seq.map (fun str -> str.Split(" ") |> Seq.map (int) |> Seq.toList)

        let output = input |> Seq.filter isDescendingAscending |> Seq.length

        let part2 =
            input
            |> Seq.filter (fun els -> not (isDescendingAscending els))
            |> Seq.filter hasApplicableSubset
            |> Seq.length

        let someList = [ 1; 9; 2; 3 ]
        hasApplicableSubset someList |> (printf "test: %A \n")
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
                printfn "%s\n" command

                if command = "don't()" || command = "do()" then
                    applyTuple <- command = "do()"
                else
                    ()
            else
                () // Soory

            let closureIndex = s.IndexOf(')', i)

            if el <> 'm' then
                None
            else
                let candidateTuple = s.Substring(i, (closureIndex - i))
                let isValid = candidateTuple.Split(",")

                let validTuple =
                    if candidateTuple.Contains("mul(") then
                        candidateTuple.Replace("mul(", "")
                    else
                        ""

                if Seq.length isValid = 2 && validTuple <> "" && applyTuple then
                    let isValid2 =
                        fst (Int64.TryParse(validTuple.Split(",").[0]))
                        && fst (Int64.TryParse(validTuple.Split(",")[1]))

                    let result () =
                        validTuple.Split(",") |> Seq.map (int) |> Seq.reduce (fun x acc -> x * acc)


                    if isValid2 then Some(validTuple, result ()) else None
                else
                    None)

    let solve filePath =
        let input = File.ReadLines filePath

        let output =
            input
            |> String.concat ""
            |> findMul
            |> (Seq.filter (Option.isSome))
            |> (Seq.map (fun el -> el |> Option.get |> snd))
            |> (Seq.sum)




        printf "\ninput: %A \n" input
        output |> (printf "part1: %A \n")

module Day4 =
    let scan (grid: string List) (x, y) =
        let rows = grid.Length
        let columns = grid[0].Length

        // walks in one direction,collecting every char, up to the limit; could also be passed as a prop
        let walk (startRow, startCol) (stepRow, stepCol) =
            let rec collectWords (r, c) acc =
                if r >= 0 && r < rows && c >= 0 && c < columns && String.length acc <> 2 then
                    collectWords (r + stepRow, c + stepCol) (acc + (grid[r][c]).ToString())
                else
                    acc

            collectWords (startRow, startCol) ""

        // Starting from the centre walk in desired directions
        seq {
            // yield walk (x,y) (0,1)
            // yield walk (x,y) (0,-1)
            // yield walk (x,y) (1,0)
            // yield walk (x,y) (-1,0)
            yield walk (x, y) (1, 1) // right down
            yield walk (x, y) (1, -1) // left down
            yield walk (x, y) (-1, -1) // left up
            yield walk (x, y) (-1, 1) // right up
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
                        if input[row][col] = 'A' then
                            yield scan input (row, col)
            }
            // |> Seq.concat
            // |> Seq.filter (fun el -> el = "XMAS" || el = "SAMX") // Part one
            // |> Seq.filter (fun el -> el |> Seq.forall (fun x -> x = "AS" || x = "AM") )
            |> Seq.filter (fun el ->
                el |> Seq.filter (fun x -> x = "AS") |> Seq.length = 2
                && el |> Seq.filter (fun x -> x = "AM") |> Seq.length = 2)
            |> Seq.map (Seq.map (fun el -> el |> Seq.last)) // The two M's and S's must be adjacent. It's not a valid cross if MAM, or SAS; leaving the only valid cases as below
            |> Seq.filter (fun el ->
                match el |> Seq.toList with
                | [ 'S'; 'S'; 'M'; 'M' ] -> true
                | [ 'M'; 'S'; 'S'; 'M' ] -> true
                | [ 'M'; 'M'; 'S'; 'S' ] -> true
                | [ 'S'; 'M'; 'M'; 'S' ] -> true
                | _ -> false)
        // |> Seq.concat

        printf "\ninput: %A \n" input
        output |> Seq.iter (printf "part1: %A \n")
        output |> Seq.length |> (printf "output: %A \n")


module Day5 =
    open System.Collections.Generic
    // Build the graph representing the rules
    let buildGraph rules =
        let adjacencyMatrix = new Dictionary<string, string list>()
        let incomingEdges = new Dictionary<string, int>() // Say rules: a | b, b | c, a | c then {a: 0, b: 1, c: 2} because it's the count of the incoming edges (or degree)

        rules
        |> Seq.iter (fun (a, b) ->
            if (adjacencyMatrix.ContainsKey(a) |> not) then
                adjacencyMatrix.Add(a, [])

            if (adjacencyMatrix.ContainsKey(b) |> not) then
                adjacencyMatrix.Add(b, [])

            if (incomingEdges.ContainsKey(a) |> not) then
                incomingEdges.Add(a, 0)

            if (incomingEdges.ContainsKey(b) |> not) then
                incomingEdges.Add(b, 0)

            adjacencyMatrix[a] <- adjacencyMatrix[a] @ [ b ]
            incomingEdges[b] <- incomingEdges[b] + 1)

        adjacencyMatrix, incomingEdges

    // Do a topological sort of graph so we have something to check against when validating the number sequences
    let doToplogicalSort (graph: Dictionary<string, string list>, inDegrees: Dictionary<string, int>) =
        let rec doDequeue queue acc =
            match queue with
            | [] -> acc
            | currentNode :: tail ->
                let neighbours = graph[currentNode]

                let enQueue =
                    neighbours
                    |> List.map (fun node ->
                        inDegrees[node] <- inDegrees[node] - 1
                        node, inDegrees[node])
                    |> List.filter (fun (_, degree) -> degree = 0)
                    |> List.map fst

                doDequeue (tail @ enQueue) (acc @ [ currentNode ])

        let initialQueue =
            inDegrees
            |> Seq.filter (fun rule -> rule.Value = 0)
            |> Seq.map (fun el -> el.Key)
            |> Seq.toList

        let sortedList = doDequeue initialQueue []

        // printf "\n%A" sortedList

        if List.length sortedList = Seq.length graph then
            Some(sortedList)
        else
            None
    
    // i don't think I need to do this. I.e. the topological sort can find these I think...
    let doPart1 numbers rulesMap = 
        rulesMap
        |> Seq.forall (fun (a, b) ->
            let aIndex = numbers |> Seq.tryFindIndex (fun el -> el = a)
            let bIndex = numbers |> Seq.tryFindIndex (fun el -> el = b)
            if aIndex.IsSome && bIndex.IsSome then aIndex < bIndex else true)
    let solvePart1 updates rulesMap  =
        updates
        |> Seq.map (fun (el: string) -> el.Split(",") |> List.ofArray)
        |> Seq.map (fun numbers ->
            let isValid = doPart1 numbers rulesMap
            let midValue = 
                if isValid then
                    (int) numbers[numbers.Length / 2]
                else
                    0
            isValid,numbers,midValue
        )
        // |> Seq.sum
    let findSubGraph numbers (graph: Dictionary<string, string list>) =
        let subGraph = new Dictionary<string, string list>()
        let indegree = new Dictionary<string, int>()

        for number in numbers do
            if (not (subGraph.ContainsKey number)) then subGraph[number] <- [] 
            if (not (indegree.ContainsKey number)) then indegree[number] <- 0 
            let neighbours = graph[number]
            for neighbour in neighbours do
                if (Seq.contains neighbour numbers) then 
                    subGraph[number] <- subGraph[number] @ [neighbour] 
                    if (not (indegree.ContainsKey neighbour)) then indegree[neighbour] <- 0
                    indegree[neighbour] <- indegree[neighbour] + 1 

        subGraph,indegree


    let solve filePath =
        let input = File.ReadLines filePath |> Seq.toList

        let splitIndex = input |> Seq.findIndex (fun el -> el.Length = 0)

        let (rules, updates) =
            (input |> Seq.take splitIndex, input |> Seq.skip (splitIndex + 1))

        let rulesMap =
            rules
            |> Seq.map (fun el ->
                match el.Split("|") with
                | [| a; b |] -> Some(a.Trim(), b.Trim())
                | _ -> None)
            |> Seq.choose id

        printf "part1: %A \n" ((solvePart1 updates rulesMap) |> Seq.filter (fun (valid,_,_) -> valid) |> Seq.map (fun (_,_,mid) -> mid) |> Seq.sum)        
        let graph, _ = rulesMap |> buildGraph
        let part2Inputs = (solvePart1 updates rulesMap) |> Seq.filter (fun (valid,_,_) -> not valid) |> Seq.map (fun (_,numbers,mid) -> numbers)
        let part2Answer = 
            part2Inputs
            |> Seq.map (fun numbers -> findSubGraph numbers graph)
            |> Seq.map (doToplogicalSort)
            |> Seq.map (fun numbers -> 
                match numbers with
                | Some(numbers) -> (int) numbers[numbers.Length / 2]
                | None -> 0
            )
            |> Seq.sum

        printf "part2 Input: %A \n" part2Inputs
        printf "part2 answer: %A \n" part2Answer

module Day6 =
    open System.Collections.Generic

    let rotateRight direction = 
        match direction with
        | (0,-1) -> (1,0) // up to right
        | (1,0) -> (0,1) // right to down
        | (0,1) -> (-1,0) // down to left
        | (-1,0) -> (0,-1) // left up
        | _ -> (0,0)
    let checkChar (x,y) grid = 
        if y > List.length grid - 1 || y < 0  then None
        else 
            let row = grid |> List.item y

            if x > List.length row - 1 || x < 0 then None
            else Some(row |> List.item x) // Some char at location (x,y)

    let checkChar2 (x,y) (grid: char array array) = 
        if y > Array.length grid - 1 || y < 0  then None
        else 
            let row = grid |> Array.item y

            if x > Array.length row - 1 || x < 0 then None
            else Some(row |> Array.item x) // Some char at location (x,y)

    let rec checkForCycle (currentX,currentY) direction (stepCoordinates: HashSet<int*int*int*int>) (grid: char array array) =       
        let dirX,dirY = direction
        let nextLocation = (currentX + dirX, currentY + dirY)
        let nextChar = checkChar2 nextLocation grid

        if stepCoordinates.Contains(currentX, currentY,dirX,dirY) then
            true
        else
            stepCoordinates.Add(currentX,currentY,dirX,dirY) |> ignore
            match nextChar with
            | Some('^') | Some('.') -> checkForCycle nextLocation direction stepCoordinates grid
            | Some('#') -> checkForCycle (currentX, currentY) (rotateRight direction) stepCoordinates grid
            | _ | None -> false // boundary
    
    /// Use only the unique positions of each visited cell. 
    /// If the direction is retained, multiple blocks are placed in the same position causing extra cycles to be counted.
    let solvePart2 initialLocation grid path = 
        let gridArray = grid |> List.map (Array.ofList) |> Array.ofList
        
        let updateGridInPlace (grid: char array array) (x, y) char =
            let original = grid[y][x]
            grid[y][x] <- char
            original
            
        let cycles = 
            path 
            |> Seq.fold (fun acc (x,y) -> 
                let original = updateGridInPlace gridArray (x, y) '#'
                let hasCycle = checkForCycle initialLocation (0,-1) (HashSet()) gridArray
                updateGridInPlace gridArray (x, y) original |> ignore
                printfn "position %A" (x,y,hasCycle)
                if hasCycle then acc + 1 else acc
            ) 0
        
        cycles

    let solvePart1 initialLocation grid = 
        let rec walkInDirection (currentX,currentY) direction (stepCoordinates: HashSet<int*int*int*int>) =       
            let dirX,dirY = direction
            let nextLocation = (currentX + dirX, currentY + dirY)
            let nextChar = checkChar nextLocation grid

            printfn ("currentLocation %A") (currentX,currentY,direction)

            stepCoordinates.Add(currentX,currentY,dirX, dirY) |> ignore

            match nextChar with
            | Some('^') | Some('.') -> walkInDirection nextLocation direction stepCoordinates
            | Some('#') -> walkInDirection (currentX, currentY) (rotateRight direction) stepCoordinates
            | _ | None -> stepCoordinates
        
        let positionsTraversed = walkInDirection initialLocation (0,-1) (HashSet())   

        positionsTraversed

    let solve filePath =
        let input = File.ReadLines filePath |> Seq.map (Seq.toList) |> Seq.toList
        let startingPosition = 
            input
            |> List.findIndex(List.contains '^') // find row
            |> (fun index -> input |> List.item (index), index)
            |> (fun (row,rowIndex) -> (row |> Seq.findIndex (fun el -> el = '^')),rowIndex) // find column

        let traversedPositions = solvePart1 startingPosition input 

        let part1 = traversedPositions |> Seq.map (fun (a,b,_,_) -> (a,b)) |> Seq.distinct 
        part1 |> Seq.length |> (printf "part 1: %A \n")
        solvePart2 startingPosition input (HashSet(part1)) |> (printf "part 2: %A \n")



module Day7 =

    let operations = [|
        (+);
        (fun a -> fun b -> a * b);
        (fun (a: int64) -> (fun (b: int64) -> int64(a.ToString() + b.ToString()))); // Part 2 solve
    |]
    let operationsSymbols = [|'+';'*'|]

    let generateOperationSequence numbers operations = 
        let n = (Seq.length numbers) - 1 // e.g. Seq.length [a,b,c,d] - 1 = 4 - 1 = 3 
        let k = Seq.length operations
        let range = (float)k**n |> int
        seq {
            for i in 0..range-1 do
                let mutable x = i // base 2 of index - flips between 0 and 1
                seq {
                    for _ in 1..n do
                        yield x % k
                        x <- x / k
                }
                |> List.ofSeq
        }
        |> List.ofSeq 
        

    let applyOperations (numbers: int64 seq) operations =
        Seq.fold2 (fun acc num op -> op acc num) (Seq.head numbers) (Seq.tail numbers) operations

    let solve filePath =
        let input = 
            File.ReadLines filePath
            |> Seq.map (fun el -> 
                let parts = el.Split(":")

                match parts with
                | [|a;b|] -> Some((int64)a, b.Trim().Split(" ") |> Array.map (int64)) // (target,numbers = [||])
                | _ -> None
            )
            |> Seq.choose id

        // For number sequence 81 40 27 apply every possible combination of operations
        // All possible combinations 81 40 27
        // (81 * 40) * 27 = ? 
        // (81 * 40) + 27 = ?
        // (81 + 40) * 27 = ?
        // (81 + 40) + 27 = ?

        // All possible combinations 11 6 16 20
        // 11 * 6 * 16 * 20
        // 11 + 6 * 16 * 20
        // 11 + 6 + 16 * 20
        // 11 + 6 + 16 + 20
        // 11 * 6 + 16 + 20
        // 11 * 6 * 16 + 20



        let output = 
            input
            |> Seq.map (fun (target,numbers) ->
                let operationSeq = generateOperationSequence numbers operations
                let hasMatch = 
                    operationSeq
                    |> List.tryFind (fun opPositions -> 
                        let opSeq = opPositions |> Seq.map (fun i -> operations[i])
                        target = applyOperations numbers opSeq
                    )
                    |> Option.isSome

                
                target,numbers,hasMatch
            )
            |> Seq.filter (fun (a,b,c) -> c = true)
            |> Seq.fold (fun acc (a,b,c) -> acc + a) ((int64)0)

        printf "\n input: %A" input  
        output |> (printf "\n output: %A") 
        
        
        // let testNumbers = [|11; 6; 16; 20|]
        // let operationSeq = generateOperationSequence testNumbers operations
        // let testResults = 
        //     operationSeq
        //     |> Seq.map (fun ops ->
        //         let aOps = ops |> Seq.map (fun i -> operations[i]) 
        //         let bOps = ops |> Seq.map (fun i -> operationsSymbols[i]) 
        //         applyOperations testNumbers aOps, ops, bOps
        //     )
        
        // testResults |>Seq.iter (printf "\n test: %A ")


module Day8 =

    let solve filePath =
        let input = 
            File.ReadLines filePath
        
        
        let output = 
            input
            

        printf "\ninput: %A \n" input
        output |> Seq.iter (printfn "output: %A \n")



// ------------------------------ TEMPLATE ------------------------------ //
module Template =

    let solve filePath =
        let input = 
            File.ReadLines filePath
        
        
        let output = 
            input
            

        printf "\ninput: %A \n" input
        output |> Seq.iter (printfn "output: %A \n")