open System
open System.IO

let ids = File.ReadAllLines("FSharp/02-inventory-input.txt")
          |> Array.map Seq.toList

// Part 1
type CountsFor =
| Neither
| Twos
| Threes
| Both

let countsFor str =
    let occurrences = Map.empty<char, int>

    let rec countsForRec str occurrences =
        match str with
        | [] -> let hasTwo = Map.exists (fun _ v -> v = 2) occurrences
                let hasThree = Map.exists (fun _ v -> v = 3) occurrences
                match (hasTwo, hasThree) with
                | (true, true) -> Both
                | (true, false) -> Twos
                | (false, true) -> Threes
                | (false, false) -> Neither
        | h :: t -> let newCount = match Map.tryFind h occurrences with
                                   | Some c -> c + 1
                                   | None -> 1
                    countsForRec t (Map.add h newCount occurrences)

    countsForRec str occurrences

let counts = ids |> Array.map countsFor

let twos = counts |> Array.filter (fun i -> i = Twos || i = Both) |> Array.length
let threes = counts |> Array.filter (fun i -> i = Threes || i = Both) |> Array.length

let result1 = twos * threes

// Part 2
let countDiffs id1 id2 =
    let rec countDiffsRec id1 id2 acc =
        match (id1, id2) with
        | (h1 :: t1, h2 :: t2) -> countDiffsRec t1 t2 (if h1 = h2 then acc else acc + 1)
        | ([], []) -> acc
    countDiffsRec id1 id2 0

let commonPart id1 id2 = List.fold2 (fun acc c1 c2 -> if c1 = c2 then acc + (string c1) else acc) "" id1 id2

let findSolution ids =
    let rec findSolutionRec ids allIds =
        match ids with
        | [] -> ""
        | h :: t -> match allIds |> List.tryFind (fun id -> (countDiffs h id) = 1) with
                    | None -> findSolutionRec t allIds
                    | Some id -> commonPart h id
    findSolutionRec ids ids

let result2 = findSolution (ids |> Array.toList)

