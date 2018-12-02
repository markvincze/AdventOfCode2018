open System.IO

let ids = File.ReadAllLines("FSharp/02-inventory-input.txt")

// Part 1
type CountsFor =
| Neither
| Twos
| Threes
| Both

let countsFor (str : string) =
    let occurrences = Seq.fold
                          (fun occ c -> Map.add c (Map.tryFind c occ |> Option.fold (fun _ v -> v + 1) 1) occ)
                          Map.empty<char, int>
                          str

    let hasTwo = Map.exists (fun _ v -> v = 2) occurrences
    let hasThree = Map.exists (fun _ v -> v = 3) occurrences
    match (hasTwo, hasThree) with
    | (true, true) -> Both
    | (true, false) -> Twos
    | (false, true) -> Threes
    | (false, false) -> Neither

let counts = ids |> Array.map countsFor

let twos = counts |> Array.filter (fun i -> i = Twos || i = Both) |> Array.length
let threes = counts |> Array.filter (fun i -> i = Threes || i = Both) |> Array.length

let result1 = twos * threes

// Part 2
let countDiffs id1 id2 =
    Seq.fold2 (fun diff c1 c2 -> if c1 = c2 then diff else diff + 1) 0 id1 id2

let commonPart id1 id2 =
    Seq.fold2 (fun acc c1 c2 -> if c1 = c2 then acc + (string c1) else acc) "" id1 id2

let findSolution ids =
    ids |> Array.pick (fun id -> ids
                                 |> Array.tryFind (fun innerId -> (countDiffs id innerId) = 1)
                                 |> Option.map (commonPart id))

let result2 = findSolution ids
