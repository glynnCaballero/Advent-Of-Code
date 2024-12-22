namespace Solutions._2023

open System.IO


module public Day5 =

    let splitIntoThree (s: string) =
        match s.Split(" ") with
        | [| a; b; c |] -> Some(int64 (a), int64 (b), int64 (c))
        | _ -> None

    let turnToTuple s =
        match s with
        | [| a; b |] -> Some(int64 (a), int64 (b))
        | _ -> None

    let toTuples el =
        el
        |> Seq.tail // take out map title e.g. ["seed-to-soil map"; "50 98 2"...] into ["50 98 2"...]
        |> Seq.map (splitIntoThree)
        |> Seq.choose id

    let applyRange seeds ranges condition =
        seeds
        |> Seq.map (fun seed ->
            match ranges |> Seq.tryFind (condition seed) with
            | Some(a, b, c) -> seed + (a - b)
            | None -> seed)

    let rec applyRangePart2 (startSeed: int64, seedRange: int64) chunk processedSeeds =
        if (seedRange <= 0) then
            processedSeeds
        else
            let applicableRange =
                chunk
                |> Seq.tryFind (fun (rangeValue, rangeStart, range) ->
                    let intersectStart = max startSeed rangeStart
                    let intersectEnd = min (startSeed + seedRange - 1L) (rangeStart + range - 1L)
                    intersectStart <= intersectEnd)

            match applicableRange with
            | Some(rangeValue, rangeStart, range) ->
                let intersectStart = max startSeed rangeStart
                let intersectEnd = min (startSeed + seedRange - 1L) (rangeStart + range - 1L)
                let intersectRange = (intersectEnd - intersectStart) + 1L
                let operation = rangeValue - rangeStart

                let mappedSeed = (intersectStart + operation, intersectRange)

                let remainingRange = seedRange - intersectRange

                let newStartSeed =
                    if intersectStart > startSeed then
                        startSeed
                    else
                        intersectEnd + 1L

                let newProcessedSeeds = processedSeeds @ [ mappedSeed ]

                if remainingRange > 0 then
                    applyRangePart2 (newStartSeed, remainingRange) chunk newProcessedSeeds
                else
                    newProcessedSeeds
            | None -> processedSeeds @ [ (startSeed, seedRange) ]

    let solve filePath =
        let input = File.ReadLines filePath

        let part1Seeds =
            Seq.head input |> (fun el -> el.Split " ") |> Seq.tail |> Seq.map int64

        let part2Seeds =
            part1Seeds |> Seq.chunkBySize 2 |> Seq.map turnToTuple |> Seq.choose id

        // Group together mappings with title above them.
        let maps =
            input
            |> Seq.tail
            |> Seq.fold
                // The return type of fold is it's accumulator - which comes from the previous call
                (fun (acc, rangeGroupings) item ->
                    if item.Length = 0 then
                        // Gap before next map groupings. Append collected ranges into accumulator.
                        (List.rev rangeGroupings :: acc, [])
                    else
                        // Group all the ranges including the title.
                        (acc, item :: rangeGroupings))
                ([], [])
            |> (fun (a, b) -> List.rev b :: a) // The last chunk doesn't have the empty space above the title
            |> Seq.filter (fun chunk -> chunk <> [])
            |> Seq.rev
            |> Seq.map toTuples

        // answer part 1: for each seed find map cluster and apply operation if any; any values shifted gets used in the next map cluster, else removed.
        let part1Answer =
            maps
            |> Seq.fold
                (fun seeds mapCluster ->
                    applyRange seeds mapCluster (fun seed (a, b, c) -> seed >= b && seed <= (b + c)))
                part1Seeds
            |> Seq.min

        // answer part 2: for each seed range (L,R), find the interecting seed values (destinationValue,x,y) from the mapping chunks e.g. Seed-to-soil...
        // if intersect, apply mapping operation to intersected values; else seed range stays and no operation is applied;
        // If partial intersect, process remaining seed values before the intersect or after the intersect;
        // move to next seed
        // https://excalidraw.com/#json=X4pR1JpoNc76sGF93n9nE,hTpaDTdMPPhpN_m0xKCTuA
        printf "Part 2 Input %A" part2Seeds

        let mutable part2Answer = Seq.toList part2Seeds

        for chunk in maps do
            printf "\n chunk %A" chunk
            let mutable processedSeeds = []

            for (startSeed, seedRange) in part2Answer do
                processedSeeds <- applyRangePart2 (startSeed, seedRange) chunk processedSeeds

            part2Answer <- processedSeeds
            printf "\n processed seeds %A" part2Answer

        printf "\n --------------- Answer --------------- \n"
        printf "Part 1: %A" part1Answer
        printf "\nPart 2: %A" (part2Answer |> Seq.map (fun (seedStart, seedRange) -> seedStart) |> Seq.min)
        printf "\n Seed count: %A" (part2Answer |> Seq.map (fun (seedStart, seedRange) -> seedRange) |> Seq.sum)

module Day6 =

    // ChaptGPT Suggested Optimal solution - a binary search for start to n instead of 1 to n.
    // Try to find the lowest x of (time - x) * x that produces a greater value than the target.
    let solvePart1 (time, target) =
        let times = [ for x in 1L .. time -> (time - x) * x ]
        times |> List.where (fun el -> el > target) |> List.length

    let solve filePath =
        let input =
            File.ReadLines filePath
            |> Seq.map (fun string -> string.Split(" "))
            |> Seq.map (fun row -> row |> Seq.where (fun string -> string.Length > 0))

        // The two rows of the input. Time:, and Distance
        let time = Seq.head input
        let distance = Seq.last input

        let part1Answer =
            Seq.zip time distance
            |> Seq.tail
            |> Seq.map (fun (time, target) -> (int64 time, int64 target))
            |> Seq.map solvePart1
            |> Seq.reduce (fun acc el -> acc * el)

        // output |> Seq.iter (printf "\n%A")
        part1Answer |> (printf "\n Part 1 Answer: %A")

        let part2Time = time |> Seq.tail |> Seq.reduce (fun acc el -> acc + el)
        let part2Distance = distance |> Seq.tail |> Seq.reduce (fun acc el -> acc + el)
        let part2Answer = solvePart1 (int64 part2Time, int64 part2Distance)
        printfn "\n Part 2%A" (part2Time, part2Distance, part2Answer)

module Day7 =
    let findCardValue c =
        match c with
        | 'A' -> 14
        | 'K' -> 13
        | 'Q' -> 12
        | 'J' -> 11
        | 'T' -> 10
        | x -> int x - int '0'

    let countCards s = 
        s
        |> Seq.fold (fun acc ch -> 
            match Map.tryFind ch acc with 
            | Some count -> Map.add ch (count+1) acc
            | None -> Map.add ch 1 acc
        ) Map.empty

    let solve filePath =
        let input =
            File.ReadLines filePath
            |> Seq.map (fun string -> string.Split(" "))
            |> Seq.choose (fun element ->
                match element with
                | [| hand; amount |] -> Some(hand, amount)
                | _ -> None)

        // Solution: Apply the second grouping 33332 > 2AAAA, because it's first card is stronger. If the first cards are the same, then highest second card
        // The first ranking is AAAAA > AAAAB > AAABB > AABBB
        let output = 
            input 
            |> Seq.map (fun (hand, amount) -> (countCards hand, int amount))
            // |> Seq.sortBy (fun (handTotal,_) -> handTotal)
            // |> Seq.reduce


        printf "input: %A \n" input
        printf "output: "
        output |> Seq.iter (printf "\n %A")

namespace Solutions
open System.IO
// ------------------------------ TEMPLATE ------------------------------ //
module Template =

    let solve filePath =
        let input = File.ReadLines filePath
        let output = input

        printf "\ninput: %A \n" filePath
        printf "DEMO output: %A \n" output
