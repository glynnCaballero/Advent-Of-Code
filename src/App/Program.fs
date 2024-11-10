open Utilities

[<EntryPoint>]
let main args =
    // printf "%d" args.Length
    let input = if args.Length = 1 then $".\\problems\\{args[0]}.txt"  else ".\\problems\\day6Demo.txt"
    
    // Choose module based on the filename (e.g., "day5.txt" -> Solutions.Day5.solve)
    match args with
    | [| "day5" |] -> RunWithTimer (fun _ -> Solutions.Day5.solve input)
    | [| "day6" |] -> RunWithTimer (fun _ -> Solutions.Day6.solve input)
    | _ -> printfn "Invalid or missing argument. Please specify a valid day module."
     

    0 // return an integer exit code