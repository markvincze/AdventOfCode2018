open System
open System.IO

let units = File.ReadAllText("FSharp/05-alchemical-input.txt")
            |> Seq.toList

// Part 1
let opposite unit =
    if unit |> Char.IsLower
    then unit |> Char.ToUpper
    else unit |> Char.ToLower

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