module Utilities

open System;

let RunWithTimer a =
    let stopWatch = Diagnostics.Stopwatch.StartNew()

    let _ = a ()

    stopWatch.Stop()
    printf "\n ------- Total Time ------- \n %d milliseconds" stopWatch.Elapsed.Milliseconds
