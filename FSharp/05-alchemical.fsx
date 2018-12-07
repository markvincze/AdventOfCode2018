open System
open System.IO

let units = File.ReadAllText("FSharp/05-alchemical-input.txt")
            |> Seq.toList

// Part 1
let opposite unit =
    if unit |> Char.IsLower
    then unit |> Char.ToUpper
    else unit |> Char.ToLower

// My initial naive solution:
let rec triggerOnce units =
    match units with
    | [] -> [], false
    | first :: t -> match t with
                    | [] -> (units, false)
                    | second :: rest -> if first = (opposite second)
                                        then (rest, true)
                                        else let result, triggered = triggerOnce t
                                             first :: result, triggered

let rec processUnits units =
    match triggerOnce units with
    | newUnits, true -> processUnits newUnits
    | newUnits, false -> newUnits

let result1 = processUnits units
              |> List.length

// Part 2
let unitTypes = "abcdefghijklmnopqrstuvwxyz" |> Seq.toList

let withoutUnit units unit = units |> List.filter (fun u -> u <> unit && (opposite u) <> unit)

let result2 = unitTypes
              |> List.map (fun u -> withoutUnit units u
                                    |> processUnits
                                    |> List.length)
              |> List.min


// This is the solution I wrote based on solutions I found on Reddit.
// Part 1
let processUnit acc ch =
     match acc with
     | h :: t when h = (opposite ch) -> t
     | xs -> ch :: xs

let solve units = List.fold processUnit [] units |> List.length
let result1B = solve units

// Part 2
let result2B = unitTypes
              |> List.map (fun u -> withoutUnit units u |> solve)
              |> List.min