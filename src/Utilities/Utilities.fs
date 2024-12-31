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
