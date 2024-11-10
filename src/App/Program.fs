open Utilities

[<EntryPoint>]
let main args =
    // printf "%d" args.Length
    let input = if args.Length = 1 then $".\\problems\\{args[0]}.txt"  else ".\\problems\\day5Demo.txt"
    
    RunWithTimer (fun _ -> Solutions.Day5.solve input) 
     

    0 // return an integer exit code