open System
open System.IO

let units = File.ReadAllText("FSharp/05-alchemical-input.txt")
            |> Seq.toList

// let unitsSmall = "dabAcCaCBAcCcaDA" |> Seq.toList
// let unitsSmall2 = "abcdefFEDjkl" |> Seq.toList
// let unitsSmall3 = "abcxvVzZXC" |> Seq.toList
// let unitsSmall4 = "abcCdefghijJklL" |> Seq.toList
let unitsSmall5 = "TgGtfFoO" |> Seq.toList
// let unitsSmall5 = "rpSsrRAaPgGTgGtfFoOUuRqQFhQqvVcCHzZJvbBVYydDjKkquUtTQLzZlIvVPlLOopLEelWSswzZdDbBLAPqQpaiIlOmkKeQqNnQ" |> Seq.toList
// let unitsSmall6 = "rpSsrRAaPgGTgGtfFoOUuRqQFhQqvVcCHzZJvbBVYydDjKkquUtTQLzZlIvVPlLOopLEelWSswzZdDbBLAPqQpaiIlOmkKeQqNnQgGzZqEJjgeDdEcCSsyYpfFPSsAzZMmhHwWmMTtaKAakAkiIKfFLWwljJzZIifFaGMHZzswWdrRDSptTNnfFRXxrxXPIwWibBeGeQ" |> Seq.toList


// Part 1
let opposite unit =
    if unit |> Char.IsLower
    then unit |> Char.ToUpper
    else unit |> Char.ToLower

// let rec triggerOnce units =
//     match units with
//     | [] -> [], false
//     | first :: t -> match t with
//                     | [] -> (units, false)
//                     | second :: rest -> if first = (opposite second)
//                                         then (rest, true)
//                                         else let result, triggered = triggerOnce t
//                                              first :: result, triggered

// let mutable reccnt = 0

let rec trigger units =
    // reccnt <- reccnt + 1
    // printfn "%s" (new String(units |> Array.ofList))
    match units with
    | [] -> [], false
    | [ _ ] -> units, false
    | first :: (second :: rest) -> if first = (opposite second)
                                   then (trigger rest |> fst, true)
                                   else let result, triggered = trigger (second :: rest)
                                        if triggered
                                        then trigger (first :: result)
                                        else first :: result, false


// let rec processUnits units =
//     match triggerOnce units with
//     | newUnits, true -> processUnits newUnits
//     | newUnits, false -> newUnits

// let result1_old = processUnits unitsSmall5
//                   |> List.length

// let result1 = trigger units
//               |> fst
//               |> List.length

// let resultSmall = trigger unitsSmall
// let resultSmall2 = trigger unitsSmall2
// let resultSmall3 = trigger unitsSmall3
// let resultSmall4 = trigger unitsSmall4
                //    |> fst
                //    |> List.length
let resultSmall5 = trigger unitsSmall5 |> fst |> List.length
let resultSmall = trigger units |> fst |> List.length
// let resultSmall6 = trigger unitsSmall6 |> fst |> List.length


// Part 2
let unitTypes = "abcdefghijklmnopqrstuvwxyz" |> Seq.toList

let withoutUnit units unit = units |> List.filter (fun u -> u <> unit && (opposite u) <> unit)

// let result2 = unitTypes
//               |> List.map (fun u -> withoutUnit units u
//                                     |> processUnits
//                                     |> List.length)
//               |> List.min

let result2 = unitTypes
              |> List.map (fun u -> withoutUnit units u
                                    |> trigger
                                    |> fst
                                    |> List.length)
              |> List.min