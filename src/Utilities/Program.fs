﻿module Utilities

open System;

let RunWithTimer a =
    let stopWatch = Diagnostics.Stopwatch.StartNew()

    let output = a ()

    stopWatch.Stop()
    printf "\n ------- Total Time ------- \n %d milliseconds" stopWatch.Elapsed.Milliseconds
    // printf "\n ------- outcome ------- \n %A " output
