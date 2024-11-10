namespace Solutions

open System.IO


module Day5 =

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
                let intersectEnd = min (startSeed + seedRange- 1L) (rangeStart + range- 1L)
                let intersectRange = (intersectEnd - intersectStart) + 1L
                let operation = rangeValue - rangeStart

                let mappedSeed = (intersectStart + operation, intersectRange)

                let remainingRange = seedRange - intersectRange
                let newStartSeed = 
                    if intersectStart > startSeed  then
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
            |> (fun (a, b) -> List.rev b :: a) // The last doesn't have the empty space above the title
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
