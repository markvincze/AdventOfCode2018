open System
open System.IO

let changes = File.ReadAllLines("01-chronal-input.txt")
              |> Array.map Int32.Parse
              |> List.ofArray

// Part 1
let rec calculateFinalFrequency current changes =
    match changes with
    | [] -> current
    | h :: t -> calculateFinalFrequency (current + h) t

let result1 = calculateFinalFrequency 0 changes

// Part 2
let rec findFirstRepeatingFrequency current changesLeft allChanges frequenciesSeen =
    if Set.contains current frequenciesSeen then
        current
    else
        match changesLeft with
        | [ last ] -> findFirstRepeatingFrequency (current + last) allChanges allChanges (Set.add current frequenciesSeen)
        | h :: t -> findFirstRepeatingFrequency (current + h) t allChanges (Set.add current frequenciesSeen)

let result2 = findFirstRepeatingFrequency 0 changes changes Set.empty