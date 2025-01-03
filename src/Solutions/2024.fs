namespace Solutions._2024
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
        if y > Seq.length grid - 1 || y < 0  then None
        else 
            let row = grid |> Seq.item y

            if x > Seq.length row - 1 || x < 0 then None
            else Some(row |> Seq.item x) // Some char at location (x,y)

    let rec checkForCycle (currentX,currentY) direction (stepCoordinates: HashSet<int*int*int*int>) (grid: char array array) =       
        let dirX,dirY = direction
        let nextLocation = (currentX + dirX, currentY + dirY)
        let nextChar = checkChar nextLocation grid

        if stepCoordinates.Contains(currentX, currentY,dirX,dirY) then
            true
        else
            stepCoordinates.Add(currentX,currentY,dirX,dirY) |> ignore
            match nextChar with
            | Some('^') | Some('.') -> checkForCycle nextLocation direction stepCoordinates grid
            | Some('#') -> checkForCycle (currentX, currentY) (rotateRight direction) stepCoordinates grid
            | _ | None -> false // boundary
    
    /// !!! Use only the unique positions of each visited cell. If the direction is retained, multiple blocks are placed in the same position causing extra cycles to be counted. !!!
    /// Walk the original path, and for every unique coordinate place a block, then check if that creates a cycle on the grid or not.
    /// Part 2 essentially uses part 1's answer and now we are going to change original path and hope (early break) that we create a cycle
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

    // Generate all sequence operations for number of "slots" between numbers.
    // numbers = [1;2;3;4] has open slots 3 (n-1), this creates 8 total combinations for 2 operations; [1;2;3] has 4 for 2 operations, 8 for 3 operations. 
    // How it works: Calculate the total operations k^(n-1). For each of those combination, shift the "k-based" counting system and yield the value.
    // e.g. k=2 i=0:[x%k=0%2=0,0/2=0%2=0,0], i=1:[1%2=1,1/2=0%2=0,0],i=2:[2%2=0,2/2=1%2=1,1/2=0%2=0], i=3:[3%2=1,3/2=1%2=1,1/2=0%2=0]
    // Basically, as i heads to 4 or 8, its normal (base 10) sequence would be (0,1,2,3) which is equal to 000,100,010,110 in base 2 (2 operations) or with three operations (000,100,200,010,110,210,020,120,220,001,011..222)
    // The resulting sequence is reverse order of the k-base equivalent, for base-2: 2 = 010, but the 
    let generateOperationSequence numbers operations = 
        let n = (Seq.length numbers) - 1
        let k = Seq.length operations
        let range = (float)k**n |> int
        seq {
            for i in 0..range-1 do
                let mutable x = i // base 2 of index - flips between 0 and 1; or base 3, cycles through 0,1,2
                seq {
                    for _ in 1..n do // equal to 0..n-1
                        yield x % k
                        x <- x / k
                }
                |> List.ofSeq
        }
        |> List.ofSeq 
        
    // This applys the sequence of operations left to right.
    // Fact! every possible base 2 (or any k) operation sequence (000,100,010,110) lengths are equal the length of numbers minus the head (say numbers = 4, minus head is 3 )
    // Because each operation sequence are the gaps between the numbers, which is equal to numbers without the first number.
    // Say numbers = [1;2;3;4] => acc = 1, tails = [2;3;4], operations = [0;1;0] = (+,*,+)
    // i = 0: acc = 1, num = 2, op = +, return acc = 1 + 2  
    // i = 1: acc = 3, num = 3, op = *, return acc = 3 * 3  
    // i = 2: acc = 9, num = 4, op = +, return acc = 9 + 4
    // acc = 13
    //  fold2 is like zip with a folder operation. Then we loop through the pair (+,2),(*,3),(+,4) with the result being used for the next call
    // initial is first number of list so (+ 2 1), (* 3 3), etc. as shown above  
    let applyOperations (numbers: int64 seq) operations =
        let acc = Seq.head numbers
        let tails = Seq.tail numbers

        Seq.zip tails operations
        |> Seq.fold (fun acc (num,op)-> op acc num) acc

        // Seq.fold2 (fun acc num op -> op acc num) (Seq.head numbers) (Seq.tail numbers) operations

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
    

///Weirdly easy. Basically group all non '.' chars. Then pick one of them, find the chars and create the direction (and distance from the current char to the "other" char)
/// For part 1. From the current char to the other (it doesn't matter which comes first, the other char to the current will create the backward propagation from the "current"). Add one additional node of equal distance of the current to the other from the other node.
///     E.g. cur = (0,0,T), other = (3,1,T): distance = (3,1) - (0,0) = (3,1). antinode coordinate = otherNode + distance = (3,1)+(3,1) = (6,2)
/// For part 2. Keep adding until the boundary. So distance (3,1), other node = (3,1), first antinode (6,2), second (9,3), etc. excluding the ones outside the boundary.
module Day8 =
    let solve filePath =
        let grid = 
            File.ReadLines filePath
            |> Seq.map (fun el -> el.ToCharArray())
        
        let length,width = Seq.head grid |> Seq.length, Seq.length grid 
            
        let input = 
            grid
            |> Seq.mapi (fun y rows -> rows |> Seq.mapi (fun x el -> if el <> '.' then Some(x,y,el) else None))
            |> Seq.map (Seq.choose id)
            |> Seq.filter (fun el -> Seq.length el > 0)
            |> Seq.concat

        let rec keepAddingUntilBoundary (x,y) (diffX,diffY) acc = 
            let canX,canY = x+diffX,y+diffY

            if ( canX <= length-1 && canX >= 0) && (canY <= width-1 && canY >= 0) then 
                keepAddingUntilBoundary (canX,canY) (diffX,diffY) ((canX,canY) :: acc)
            else 
                acc
        
        let output = 
            input
            |> Seq.map(fun ((curX,curY,curChar)) -> 
                let candidates = input |> Seq.filter(fun (x,y,char) -> char = curChar && x <> curX && y <> curY)

                let distanceToCandidateEl = candidates |> Seq.map (fun (x,y,_) -> x - curX, y - curY) |> List.ofSeq
                let antinodeCoordinates = 
                    Seq.zip candidates distanceToCandidateEl  
                    |> Seq.map (fun ((x,y,_), (diffX,diffY)) -> keepAddingUntilBoundary (x,y) (diffX,diffY) [])
                    |> List.ofSeq
                    // |> Seq.filter (fun (x,y) ->( x <= length-1 && x >= 0) && (y <= width-1 && y >= 0)) // check if antinode in boundary
                let something = candidates |> Seq.map (fun (a,b,_) -> (a,b)) |> Seq.toList
                antinodeCoordinates @ [something]
            )
            |> Seq.collect id
            |> Seq.concat
            |> Seq.distinct
            |> Seq.sortBy (fun (_,y) -> y)

            

        printf "\ninput: %A %A \n" (length,width) grid
        output |> Seq.iter (printfn "output: %A \n")
        output |> Seq.length |> (printfn "output: %A \n")
module Day9 =
    ///Chat GPT helper function, couldn't figure out how to end for the loop.
    let rec findGap nums minRange =
        let rec loop currentIndex currentGap startIndex =
            if currentIndex >= Array.length nums  then
                // end of array. Check if condition is met
                if currentGap >= minRange then Some startIndex else None
            else
                match nums.[currentIndex] with
                | "." ->
                    // If we're in a gap, increase its size
                    if currentGap = 0 then loop (currentIndex + 1) 1 currentIndex
                    else loop (currentIndex + 1) (currentGap + 1) startIndex
                | _ ->
                    // If the gap ends, check if it meets the condition
                    if currentGap >= minRange then Some startIndex
                    else loop (currentIndex + 1) 0 0 // move to next char
        loop 0 0 0

    let solvePart2 translatedChars numbers =
        let mutable mutableChars = Array.copy translatedChars
        for i in 0..(List.length numbers) - 1 do
            let (currentElement,range) = numbers |> List.item i
            let indexOfCurrentElement = translatedChars |> Array.findIndex (fun el -> el = string currentElement)
            let gap = findGap mutableChars range
            
            if gap.IsSome && gap.Value < indexOfCurrentElement then
                for k in gap.Value..gap.Value+range-1 do
                    mutableChars[k] <- string currentElement
                for k in indexOfCurrentElement..indexOfCurrentElement+range-1 do
                    mutableChars[k] <- "."
                printfn "num %A" (currentElement,indexOfCurrentElement,gap)

        
        mutableChars

    let solvePart1 inputString = 
        ///The problem with the demo solve is that it didn't keep the file id as a string. Instead concatenated the result (fid * range) as a string.
        /// So when we swapped values, say the last string was suppose to be "17", it was accessed as [|...;1;7|]. So the 7 would get swapped instead of the whole block "17".
        let rec translateString s index (result,fidList) =         
            match s with
            | [||] -> result,fidList
            | _ -> 
                let head= s |> Array.head |> string
                let tail = s |> Array.tail
                let fid,_ = if index = 0 then 0,0 else  fidList |> List.head |> (fun (a,b) -> a+1,b) 
                
                let range = int head
                let fileBlock = (index % 2) = 0
                let charToReplicate = if fileBlock then string fid else "." 
                let translatedChars = Array.init range (fun _ -> charToReplicate)
                let updatedFidList = if fileBlock then (fid,range) :: fidList else fidList 

                // printfn "index %A %A %A %A" index head range translatedChars
                let updatedResult = Array.concat([|result;translatedChars|])
                translateString tail (index + 1) (updatedResult,updatedFidList)
                

        let rec swapLastCharToFirstFreeSpace inputString = 
            let lastCharIndex = inputString |> Array.findIndexBack (fun el -> el <> ".")
            let firstFreeSpaceIndex = inputString |> Array.tryFindIndex (fun el -> el = ".")  
            if firstFreeSpaceIndex.IsNone then inputString
            elif firstFreeSpaceIndex.Value > lastCharIndex  then inputString
            else 
                inputString[firstFreeSpaceIndex.Value] <- inputString[lastCharIndex]
                inputString[lastCharIndex] <- "."

                swapLastCharToFirstFreeSpace (inputString)
            
        let translatedString, fidList = translateString inputString 0 ([||], [])
        
        let part2Ans = 
            solvePart2 translatedString fidList
            |> Seq.mapi (fun i el -> if el = "." then int64 0 else int64 el * int64 i)
            |> Seq.sum
        
        let part1Ans = 
            translatedString
            |> swapLastCharToFirstFreeSpace
            |> Seq.mapi (fun i el -> if el = "." then int64 0 else int64 el * int64 i)
            |> Seq.sum

        part1Ans,part2Ans
            
    let solve filePath =
        let input = 
            File.ReadLines filePath
            |> Seq.head
        
        let output = solvePart1 (input.ToCharArray()) 
            
            

        // printf "\ninput: %A \n" input
        output |> (printfn "output: %A \n")

module Day10 =
    let findNeighbours grid (x,y)  =
        let length = grid |> Array.length
        let width = grid |> Array.head |> Array.length
        let inBoundary (x,y) = x >= 0 && y >= 0 && x < width && y < length
        [
            (0,-1);
            (0,1); 
            (-1,0);
            (1,0)   
        ]
        |> List.map (fun (dx,dy) -> 
            let neighbourCoord = (x+dx,y+dy)
            if (neighbourCoord |> inBoundary) then Some(neighbourCoord) else None
        )
        |> List.choose id

    // This is a DFS because it exhaust the first neighbours sub tree first. Then the first neighbours first neighbours sub tree etc. then second first neighbour, the second first neighbours subtree etc.
    // BFS goes first neighbours first e.g. first neighbour, second neighbour, first neighbour of first neighbour, first neighbour of second neighbour, first neighbour of the first neighbour of the firstest neighbour etc.
    let rec findNine (grid: int array array) (x,y) path = 
        let currentElement = grid[y][x]
        let updatedPath = path @ [(x, y)]  // Extend the current path with the current coordinate.
        
        if currentElement = 9 then
            [updatedPath]  // Return the path as a single-item list.
        else
            let neighbors =
                findNeighbours grid (x, y)
                |> List.filter (fun (nx, ny) ->
                    let neighborElement = grid[ny][nx]
                    neighborElement - currentElement = 1
                )
            printfn "what %A" ((x,y),neighbors)
            
            // Exhausted paths from (0,0) to first neighbours [(0,1);(1,0)] to the target -> results in [ [] ; [(0,0),(0,1),(0,2),...,(0,4)]
            // If a non target terminal node is encountered, then it will have no neighbours. The empty list is propagating back up to the initiating call.
            // Say findNine (0,3) = [], find(0,1) = findNine(findNine (findNine (0,3))), findNine (0,2) = findNine (findNine (0,3)) = [] 
            // If the node has neighbours like the initial call findNine (0,0), the second call (0,1) would return the original path [(0,0),(0,1)] passed down to the findNine (0,4) with the (0,4) appended.
            // Collect would then filter emptys. Can be simplified by using findNine directly into the collect.
            let neighbourOfNeighbours = 
                neighbors
                |> List.map (fun neighbor -> findNine grid neighbor updatedPath)
                
            neighbourOfNeighbours |> List.iter (fun  el -> el |> printfn "ssss %A %A" (x,y))
            neighbourOfNeighbours @ [[];[]] |> List.collect id
        

    let solve filePath =
        let input = 
            File.ReadLines filePath
            |> Seq.map (fun el -> el.ToCharArray() |> Array.map (fun el ->  el |> string |> int ))
            |> Seq.toArray
        
        let startingPoints =
            input
            |> Seq.indexed
            |> Seq.fold (fun acc (y,row) ->
                let yIndices = row |>  Array.indexed |> Array.filter (fun (x,el) -> el = 0) |> Array.map (fst)
                acc @ (yIndices |> Seq.map (fun x -> (x,y)) |> List.ofSeq)
            ) []
        let output = 
            startingPoints
            |> Seq.map (fun startingPoint -> startingPoint, findNine input startingPoint [])
        let part2Ans = output |> Seq.sumBy (fun (_,paths) -> paths |> List.length)
        let part1Ans = 
            output 
            |> Seq.map (fun (sp,paths) -> 
                // We only care about the paths that reach 9
                let result = 
                    paths 
                    |> List.map (fun d -> List.last d )
                    |> List.distinct 
                    |> List.length
                sp,result
            )
            |> Seq.sumBy (fun (_,count) -> count)
        
            

        input |> Seq.iter (printf "\ninput: %A")
        printfn "\n"
        (part1Ans,part2Ans) |> (printfn "output: %A \n")

module Day11 =
    open System.Collections.Generic
    open System.Collections.Concurrent
    open FSharp.Collections.ParallelSeq

    let mutable dict = ConcurrentDictionary<int64,list<int64>>()
    let blink num1 = 
        dict.GetOrAdd(num1, fun num -> 
            if dict.ContainsKey num then
                dict[num]
            else
                let result = 
                    match num with
                    | 0L -> [1L]
                    | x when (string x).Length % 2 = 0 -> 
                        let stringNum = (string x).ToCharArray()
                        let a,b = 
                            stringNum 
                            |> Array.splitAt (stringNum.Length / 2) 
                            |> (fun (a,b) -> String.Concat a, String.Concat b)
                            |> (fun (a,b) -> int64 a, int64 b)
                        // printfn "something %A" (stringNum, stringNum.Length / 2)

                        [a;b]
                    | x -> [x*2024L]
                result
        
        )

    // The big problem with this is the size of nums. At around depth (blinkCount) > 25 it gets way too big. Which when enumerated on, I think, causes the slowness.
    // Even with the dict on the atomic lookup, it cost too much to keep in memory and keep expanding the nums collection and deriving properties off it etc.
    let rec blinkByAmount nums blinkCount blinkTarget = 
        printf "blink: %A\n" (Seq.length nums, blinkCount)
        seq {
            if blinkCount = blinkTarget then
                yield! nums
            else 
                let updatedNums = 
                    nums
                    |> PSeq.collect (fun el -> blink el)
                    |> PSeq.toList
                yield! blinkByAmount updatedNums (blinkCount + 1) blinkTarget
        }

    /// Don't actually expand the nums collection. Calculate the number of children of each num at a certain depth.
    /// Important! store the depth (blinkCount) and the num because at different depths the num will have different return values. E.g. blink2 (17,1) = 2, blink2 (17,4) = 4, blink2 (17,7) = 9 etc.   
    /// Can't do layer by layer calcs, as we no longer return the actual children, so we can't calculate the intermediate results (direct children's children) anymore. Instead blink2 aggregates the count already.
    /// The dict is key for part 2, even depth 25 without it is still too slow. The sub tree calculations are too much to recalculate at large depths.
    let rec solvePart2 nums blinkTarget =
        let mutable localDict = Dictionary<Tuple<int64,int>,int64>()
        
        let rec  blink2 num blinkCount = 
            printfn "numCalc %A" (num,blinkCount,localDict.ContainsKey((num,blinkCount)))
            if blinkCount = blinkTarget then
                1L
            elif localDict.ContainsKey((num,blinkCount)) then
                localDict[(num,blinkCount)]
            else
                let result = 
                    match num with
                    | 0L -> blink2 1 (blinkCount + 1)
                    | x when (string x).Length % 2 = 0 -> 
                        let stringNum = (string x).ToCharArray()
                        let a,b = 
                            stringNum 
                            |> Array.splitAt (stringNum.Length / 2) 
                            |> (fun (a,b) -> String.Concat a, String.Concat b)
                            |> (fun (a,b) -> int64 a, int64 b)
                        (blink2 (a) (blinkCount + 1)) + blink2 (b) (blinkCount + 1)
                    | x -> blink2 (x*2024L) (blinkCount + 1)
                localDict.Add((num,blinkCount), result)
                result
        
        let mutable result = 0L

        for root in nums do
            let childrenCount = blink2 root 0
            printfn "root: %A" (root,childrenCount)
            result <- result + childrenCount
        
        result

    let solve filePath =
        let input = 
            File.ReadLines filePath
            |> Seq.head
            |> (fun el -> el.Split(" "))
            |> Seq.map (int64)
            |> Seq.toList
        
        
        let output = blinkByAmount ( input) 0 75 |> Seq.length // part1Ans
        // let output = solvePart2 input 75
            

        printf "\ninput: %A \n" input
        output |> (printfn "output: %A \n")

module Day12 =
    open System.Collections.Generic
    let inboundary grid (x1,y1) =
        let length = grid |> Array.length
        let width = grid |> Array.head |> Array.length

        not (x1 < 0 || y1 < 0 || y1 >= length || x1 >= width)

    let getNeighbours (x,y) =  Utilities.directions |> Seq.map (fun (dx,dy) -> (x+dx,y+dy))

    let getPerimeter region  = 
        region 
        |> Seq.map (fun (_,x,y) -> Utilities.directions |> Seq.map (fun (dx,dy) -> (x+dx,y+dy)))
        |> Seq.concat
        // |> Seq.distinct // Allow duplicates for shared boundaries.
        |> Seq.filter (fun (x,y) -> region |> Seq.exists (fun (_,a,b) -> x = a  && y = b) |> not) // exclude tiles in the region

    // DFS based. Visited set to stop recalculation of region from coordinates already in a region. 
    let findRegions grid  =
        let inboundary = inboundary grid
        let mutable visited = new HashSet<Tuple<_,int,int>>()

        let rec findNeighbours coord (currentNeighbours: HashSet<Tuple<_,int,int>>)=
            let (el,x,y) = coord

            visited.Add(coord) |> ignore
            currentNeighbours.Add(coord) |> ignore

            let neighbours = 
                Utilities.directions
                |> Seq.map (fun (dx,dy) -> (x+dx,y+dy) )
                |> Seq.filter inboundary
                |> Seq.map (fun (x1,y1) -> (grid[y1][x1],x1,y1))
                |> Seq.filter (fun (c1,x1,y1) -> c1 = el && (currentNeighbours.Contains(c1,x1,y1) |> not))
                |> Seq.toList
            printfn ("Find neighbours for coord: %A") ((el,x,y),neighbours, currentNeighbours)
            for neighbour in neighbours do
                findNeighbours  neighbour currentNeighbours |> ignore
            currentNeighbours

        
        let allCoordinates = 
            grid
            |> Seq.mapi (fun y row ->
                row
                |> Seq.mapi (fun x el -> 
                    (el,x,y)
                )
            )
            |> Seq.concat
        
        let mutable result = [] 
        for coord in allCoordinates do
            if not (visited.Contains(coord)) then // To stop recalculation of already calculated region.
                let region = findNeighbours coord (new HashSet<Tuple<_,int,int>>())
                result <- region :: result
        result

    // Count the sides idea. Keep track of the boundary tiles of the region and it's excess (adjacent off region tile) for the direction of the side (horizantal or veritcal side).
    // For each side tile (inRegionTile,boundaryTile) group them together by their adjacent neighbour side tiles.
    // It's an adjacent neighbour side tile: if the inRegionTile is adjacent to the current side tile, and the boundary tile of the inRegionTile and the current tiles's boundary tile are also adjacent.
    // E.g. say we have a corner region [(0,0),(1,0),(0,1)]; we have a boundary of [((0,0), (0,-1)),((0,0),(-1,0)),((1,0),(1,-1)),((0,1),(-1,0))]
    // Say we pick ((0,0),(-1,0)), then the adjacent tiles of inRegion tile are [((1,0),(,-1)),((0,1),(-1,0))]. 
    // Find the neighbours in region tiles of (0,0) = [(1,0),(0,1)] = [((1,0),(,-1)),((0,1),(-1,0))]
    // neighbours boundary tile (-1,0) = [(-1,0)] = [((0,1),(-1,0))]
    // The applicable neighbours are the intersect of both neighbours set = [((0,1),(-1,0))] (in this case, but every side tile that appear in both lists belong to the same side)
    // Both (0,-1) side tiles are added to the same side collection and no longer considered in following iterations. Leaving only the remaining sides [((0,0), (0,-1)),((1,0),(1,-1)))]
    let solvePart2 (region: HashSet<Tuple<_,int,int>>) = 
        let inRegion (x,y) = region |> Seq.exists (fun (_,x1,y1) -> x1=x&&y1=y)
        
        let regionBoundary = 
            region
            |> Seq.collect (fun (c,x,y) -> 
                let neighbours = 
                    getNeighbours(x,y)
                    |> Seq.filter (fun coord -> inRegion(coord) |> not)

                neighbours |> Seq.map (fun coord -> ((c,x,y),coord))
            )
            |> Seq.toList

        let isRegionBoundaryNeighbours (x,y) = regionBoundary |> List.filter (fun ((_,x1,y1), _) -> x=x1&&y=y1)
        let isBoundaryNeighbours (x,y) = regionBoundary |> List.filter (fun (_, (x1,y1)) -> x=x1&&y=y1)
        let mutable visitedEdges = new HashSet<Tuple<Tuple<_,int,int>,Tuple<int,int>>>()
        let rec getGrouping edge (currentGrouping: HashSet<Tuple<Tuple<_,int,int>,Tuple<int,int>>>) = 
            let (_,x,y),boundaryCoordinate = edge
            let inRegionNeighbours = getNeighbours (x,y) |> Seq.collect (isRegionBoundaryNeighbours)
            let boundaryNeighbours = getNeighbours boundaryCoordinate |> Seq.collect (isBoundaryNeighbours)
            
            visitedEdges.Add(edge) |> ignore
            currentGrouping.Add(edge) |> ignore
            
            // printfn "Current Edge %A" edge
            // printfn "Coordinate Neighbours %A \nBoundary Neighbours %A" inRegionNeighbours boundaryNeighbours
            
            if Seq.length inRegionNeighbours = 0 || Seq.length boundaryNeighbours = 0 then 
                currentGrouping
            else
                // Not head. We could start in the middle of a side and need to add both left and right sides (because we started in the middle) on the same side (grouping)
                let applicableNeighbours = inRegionNeighbours |> Seq.filter (fun neighbourEdge -> (boundaryNeighbours |> Seq.contains neighbourEdge) && (currentGrouping.Contains(neighbourEdge) |> not) && (visitedEdges.Contains(neighbourEdge) |> not)) 
                let mutable result = currentGrouping
                for coord in applicableNeighbours do
                    result <- getGrouping coord currentGrouping
                result

        let mutable sides = []
        for edge in regionBoundary do
            if not(visitedEdges.Contains(edge)) then
                sides <- getGrouping edge (new HashSet<Tuple<Tuple<_,int,int>,Tuple<int,int>>>()) :: sides

        // printfn "side %A" (regionBoundary)

        sides |> Seq.length

    let solvePart1 input =
        let regions = findRegions input
        let result = 
            regions 
            |> Seq.map (fun region -> Seq.head region |> (fun (a,_,_) -> a), region, getPerimeter region)
            |> Seq.sortBy (fun (a,_,_) -> a)
        result |> Seq.sumBy (fun (_,area,perimeter) -> (Seq.length area) * (perimeter |> Seq.length)),result

    let solve filePath =
        let input = 
            File.ReadLines filePath
            |> Seq.map (fun el -> el.ToCharArray())
            |> Seq.toArray
        printf "\ninput: %A \n" input
        
        let part1Ans,result = solvePart1 input
        part1Ans |> (printfn "part1Ans: %A \n")
        
        let part2Ans = 
            result 
            |> Seq.sumBy (fun (char,region,_) -> 
                let part2Perimeters = solvePart2 region
                // printfn "perimeter %A" (char,Seq.length region, part2Perimeters)
                Seq.length region * part2Perimeters
            )
        part2Ans |> (printfn "part2Ans: %A \n")

module Day13 =
    let splitIntoCoordinates (s: string) (operator: string) = 
        let sSplit = s.Split(":") |> Seq.last
        let coordinates = 
            sSplit.Split(",")
            |> Seq.map (fun el -> el.Split(operator) |> Seq.last )
            |> Seq.toList
        
        match coordinates with
        | [moveX;moveY] -> (int (moveX.Trim()),int (moveY.Trim()))
        | _ -> (-1,-1)
    

    // using the tx%GCD(aX,bX)=0 didn't work because it solves each equation individually and doesn't check the consistency of both equations.
    // And still has to use the determinenet (aY*bX - aX*bY) to check (ty*bX - tx*bY) % (aY*bX - aX*bY) = 0  checks the consistency of both equations.
    let doMathStuff ((aX,aY),(bX,bY),(tx:int64,ty:int64)) = 
        // let tx = (i*aX) + (j*bX)
        // tx - (i*aX) = (j*bX)
        // (tx - (i*aX))/bX = j
        //  
        // let ty = (i*aY) + (j*bY)
        // ty = (i*aY) + (((tx - (i*aX))/bX)*bY)
        // ty = (i*aY) + (tx*bY - (i*aX*bY)/bX))
        // ty - (i*aY) =  (tx*bY - (i*aX*bY)/bX))
        // ty*bX - i*aY*bX =  tx*bY - (i*aX*bY)
        // ty*bX - tx*bY  =  i*aY*bX - i*aX*bY
        // ty*bX - tx*bY  =  i(aY*bX - aX*bY)
        // (ty*bX - tx*bY)/(aY*bX - aX*bY)  =  i
        
        let i = (ty*bX - tx*bY) / (aY*bX - aX*bY)
        let j = (tx - (i*aX))/bX
        let isValid = ((ty*bX - tx*bY) % (aY*bX - aX*bY)) = 0 && (((tx - (i*aX))%bX) = 0)
        
        if isValid then (i,j) else (0L,0L) 

    let solvePart2 input = 
            input
            |> Seq.map (fun ((aX,aY),(bX,bY),(tX,tY)) -> ((int64 aX,int64 aY),(int64 bX, int64 bY),((10000000000000L) + ((int64) tX),(10000000000000L) + ((int64) tY))))
            // |> Seq.map (fun ((aX,aY),(bX,bY),(tX,tY)) -> ((int64 aX,int64 aY),(int64 bX, int64 bY),(((int64) tX),((int64) tY))))
            |> Seq.map doMathStuff
            // |> Seq.sumBy (fun (x,y) -> 
            //     (x*3L)+y
            // )

    let solve filePath =
        let input = 
            File.ReadLines filePath
            |> Seq.chunkBySize 4
            |> Seq.map (fun chunk -> 
                match chunk with
                | [|buttonA;buttonB;target;_|] -> Some((splitIntoCoordinates buttonA "+",splitIntoCoordinates buttonB "+",splitIntoCoordinates target "="))
                | [|buttonA;buttonB;target|] -> Some((splitIntoCoordinates buttonA "+",splitIntoCoordinates buttonB "+",splitIntoCoordinates target "="))
                | _ -> None
            )
            |> Seq.choose id
        printf "\ninput: %A \n" input
        
        let part1Ans = 
            input
            |> Seq.map (fun (buttonA,buttonB,target) -> 
                let aX,aY = buttonA
                let bX,bY = buttonB
                let tX,tY = target
                let mutable result = List.empty
                for i in 1 .. 100 do
                    for j in 0 .. 100 do
                        let currentX = (i*aX) + (j*bX)
                        let currentY = (i*aY) + (j*bY)
                        if currentX = tX && currentY = tY then
                            result <- (i,j) :: result
                    
                result
            )
            |> Seq.sumBy (fun el -> 
                if Seq.length el > 1 then printfn "%A" el
                if Seq.length el > 0 then
                    let x,y = el |> Seq.head
                    (x*3)+y
                else 
                    0
            )
        part1Ans |> (printfn "part1Ans: %A \n")
        let part2Ans = solvePart2 input 

        
        part2Ans |> (printfn "\npart2Ans: %A \n")

module Day14 =

    let solve filePath =
        let input = 
            File.ReadLines filePath
        printf "\ninput: %A \n" input        
        
        let output = 
            input

        let width = 11
        let height = 7
            
        for y in 1..height do
            printf "\n"
            for x in 1..width do
                let xMid = (1 + width) / 2
                let yMid = (1 + height) / 2
                if xMid <> x && yMid <> y then
                    printf "."
                else 
                    printf " "

        // output |> Seq.iter (printfn "output: %A \n")
        
        
// ------------------------------ TEMPLATE ------------------------------ //
module Template =

    let solve filePath =
        let input = 
            File.ReadLines filePath
        printf "\ninput: %A \n" input        
        
        let output = 
            input
            

        output |> Seq.iter (printfn "output: %A \n")