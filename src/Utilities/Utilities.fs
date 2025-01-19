module Utilities
open System;

let RunWithTimer a =
    let stopWatch = Diagnostics.Stopwatch.StartNew()

    let output = a ()

    stopWatch.Stop()
    printf "\n ------- Total Time ------- \n %A" stopWatch.Elapsed
    // printf "\n ------- outcome ------- \n %A " output

let directions = 
    seq {
        (0,-1); // up
        (1,0); // right
        (0,1); // down
        (-1,0); // left
    }

let findPositionInGrid grid target = 
    grid 
    |> Seq.findIndex(fun el -> Seq.contains target el)
    |> (fun y -> 
        let row = Seq.toArray grid |>(fun g -> g[y]) 
        let x = row |> Seq.findIndex(fun el -> el = target)
        (x,y)
    )

let inBoundary (x,y) grid = 
    let length = grid |> Seq.length
    let width = grid |> Seq.head |> Seq.length
    x >= 0 && y >= 0 && x < width && y < length

let findNeighboursd grid (x,y) directions =
    let length = grid |> Array.length
    let width = grid |> Array.head |> Array.length
    let inBoundary (x,y) = x >= 0 && y >= 0 && x < width && y < length
    
    directions
    |> Seq.map (fun (dx,dy) -> 
        let neighbourCoord = (x+dx,y+dy)
        if (neighbourCoord |> inBoundary) then Some(neighbourCoord) else None
    )
    |> Seq.choose id

let findNeighbours grid coord  = findNeighboursd grid coord directions