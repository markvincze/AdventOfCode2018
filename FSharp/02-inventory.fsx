open System
open System.IO

let ids = File.ReadAllLines("FSharp/02-inventory-input.txt")

// Part 1
let counts ids =
    let rec countsRec ids twos threes =
        match ids with
        | [] -> (twos, threes)
        | h :: t -> let chars = Seq.fold (fun occ c -> Map.add c (Map.tryFind c occ |> Option.fold (fun _ v -> v + 1) 1) occ)
                                         Map.empty<char, int>
                                         h
                    countsRec t
                              (if Map.exists (fun _ v -> v = 2) chars then twos + 1 else twos)
                              (if Map.exists (fun _ v -> v = 3) chars then threes + 1 else threes)
    countsRec (Array.toList ids) 0 0

let twos, threes = counts ids

let result1 = twos * threes

// Part 2
let countDiffs id1 id2 =
    Seq.zip id1 id2
    |> Seq.filter (fun (c1, c2) -> c1 <> c2)
    |> Seq.length

let commonPart (id1 : string) (id2 : string) =
    Seq.zip id1 id2
    |> Seq.filter (fun (c1, c2) -> c1 = c2)
    |> Seq.map fst
    |> String.Concat

let findSolution ids =
    ids |> Array.pick (fun id -> ids
                                 |> Array.tryFind (fun innerId -> (countDiffs id innerId) = 1)
                                 |> Option.map (commonPart id))

let result2 = findSolution ids
