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
    let problemYear = results.GetResult(<@ Year @>, defaultValue = "2024")
    let isDemo = if demoMode then "Demo"  else ""
    let input = $".\\problems\\{problemYear}\\{problem}{isDemo}.txt" 

    printfn "%s" input

    // TODO Do the reflection thingy
    let solve =
        match problemYear with
        | "2023" ->
            match problem with
            | "day5" -> fun _ -> Solutions._2023.Day5.solve input
            | "day6" -> fun _ -> Solutions._2023.Day6.solve input
            | "day7" -> fun _ -> Solutions._2023.Day7.solve input
            | _ -> fun _ -> Solutions.Template.solve input
        | "2024" ->
            match problem with
            | "day1" -> fun _ -> Solutions._2024.Day1.solve input
            | "day2" -> fun _ -> Solutions._2024.Day2.solve input
            | "day3" -> fun _ -> Solutions._2024.Day3.solve input
            | "day4" -> fun _ -> Solutions._2024.Day4.solve input
            | "day5" -> fun _ -> Solutions._2024.Day5.solve input
            | "day6" -> fun _ -> Solutions._2024.Day6.solve input
            | "day7" -> fun _ -> Solutions._2024.Day7.solve input
            | "day8" -> fun _ -> Solutions._2024.Day8.solve input
            | "day9" -> fun _ -> Solutions._2024.Day9.solve input
            | "day10" -> fun _ -> Solutions._2024.Day10.solve input
            | "day11" -> fun _ -> Solutions._2024.Day11.solve input
            | "day12" -> fun _ -> Solutions._2024.Day12.solve input
            | "day13" -> fun _ -> Solutions._2024.Day13.solve input
            | "day14" -> fun _ -> Solutions._2024.Day14.solve input
            | "day15" -> fun _ -> Solutions._2024.Day15.solve input
            | _ -> fun _ -> Solutions.Template.solve input
        | _ -> fun _ -> Solutions.Template.solve input
    RunWithTimer (fun _ -> solve())

    0 // return an integer exit code