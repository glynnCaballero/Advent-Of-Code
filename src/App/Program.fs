open System

[<EntryPoint>]
let main args =
    // printf "%d" args.Length
    let input = if args.Length = 1 then $".\\problems\\{args[0]}.txt"  else ".\\problems\\day5Demo.txt"

    let stopWatch = Diagnostics.Stopwatch.StartNew()
    
    Solutions.Day5.solve input

    stopWatch.Stop();
    printf "\n ------- Total Time ------- \n %d milliseconds" stopWatch.Elapsed.Milliseconds

    0 // return an integer exit code