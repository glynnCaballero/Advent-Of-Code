open Utilities
open Argu

type CLIArgs =
    | Problem of string
    | Demo of bool
    | Year of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Problem _ -> "Specify the problem to run (e.g., day5)"
            | Demo _ -> "Enable or disable demo mode (default: on)"
            | Year _ -> "Problem year (e.g. 2024)"


[<EntryPoint>]
let main args =
    let results = ArgumentParser.Create<CLIArgs>().Parse(args)

    let problem = results.GetResult(<@ Problem @>, defaultValue = "")
    let demoMode = results.GetResult(<@ Demo @>, defaultValue = true)
    let problemYear = results.GetResult(<@ Year @>, defaultValue = "2023")
    let isDemo = if demoMode then "Demo"  else ""
    let input = $".\\problems\\{problemYear}\\{problem}{isDemo}.txt" 

    printfn "%s" input

    // TODO Do the reflection thingy
    let solve =
        match problem with
        | "day5" -> fun _ -> Solutions.Day5.solve input
        | "day6" -> fun _ -> Solutions.Day6.solve input
        | "day7" -> fun _ -> Solutions.Day7.solve input
        | "day1" -> fun _ -> _2024.Day1.solve input
        | "day2" -> fun _ -> _2024.Day2.solve input
        | _ -> fun _ -> Solutions.Template.solve input
    RunWithTimer (fun _ -> solve())

    0 // return an integer exit code