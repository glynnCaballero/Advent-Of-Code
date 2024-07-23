namespace Solutions

open System.IO

module Day5 =
    type Seed(id) =
        class
            let mutable value = int64 0
            let mutable isApplied = false
            member this.getValue = value
            member this.setValue v = value <- v
            member this.applied b = isApplied <- b
            member this.IsApplied = isApplied
            member this.id = int id
        end

    let indexOfString s = List.findIndex (fun el -> el = s)


    let solve filePath =
        let input = File.ReadLines filePath

        let seeds =
            Seq.head input
            |> (fun el -> el.Split " ")
            |> Seq.tail
            |> Seq.map int64
            |> Seq.toArray

        let maps = Seq.tail input |> Seq.filter (fun el -> el.Length > 0)


        let mutable output =
            [ 0 .. Seq.length (seeds) - 1 ]
            |> Seq.map Seed
            |> Seq.map (fun (seed: Seed) ->
                seed.setValue (seeds[seed.id])
                seed)


        printf "%A \n" (output |> Seq.map (fun el -> el.getValue))
        let mutable currentMap = ""

        for map in maps do
            if Seq.last map = ':' then
                currentMap <- map

                output <-
                    Seq.map
                        (fun (seed: Seed) ->
                            seed.applied false
                            seed)
                        output

                printf "\n %A" (output |> Seq.map (fun el -> el.IsApplied))
                printf "\nCurrent Map: %s \n" currentMap
            else
                let [| a; b; c |] = map.Split " " |> Array.map int64

                let operation = a - b // mapping operation e.g. seed = 98, b = 98, a = 50, then operation = -48, because, soil50 <- seed98 = 50 - 98 = -48

                let applyToElement (seed: Seed) =
                    if seed.getValue >= b && seed.getValue < (b + c) && not seed.IsApplied then
                        seed.setValue (seed.getValue + operation)
                        seed.applied true

                    seed


                output <- Seq.map applyToElement output


                printf "%A" (output |> Seq.map (fun el -> el.getValue))

        output |> Seq.map (fun el -> el.getValue) |> Seq.min |> printf "\n\n outcome: %A"


// printf "%s" seeds
// Seq.iter (fun elem -> printf "%s \n" elem) maps
