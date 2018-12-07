open System.IO
open System.Text.RegularExpressions

let lineRegex = new Regex("Step (.) must be finished before step (.) can begin.", RegexOptions.Compiled)

let parseLine line = let rm = lineRegex.Match line
                     (rm.Groups.[1].Value.[0], rm.Groups.[2].Value.[0])

let input = File.ReadAllLines("FSharp/07-sleigh-input.txt")
                     |> Array.map parseLine

let addToCache cache (a, b) =
    let withA = match Map.tryFind a cache with
                | Some _ -> cache 
                | None -> cache |> Map.add a Set.empty<char>
    match Map.tryFind b withA with
    | Some deps -> withA |> Map.add b (Set.add a deps)
    | None -> withA |> Map.add b (Set.add a Set.empty<char>)

let cache = input |> Array.fold addToCache Map.empty<char, Set<char>>

type Worker = {
    Job : char option
    WorkLeft : int
}

let newWorker = { Job = None; WorkLeft = 0 }

let tryGetNext (cache : Map<char, Set<char>>) =
    cache
    |> Seq.filter (fun kvp -> Seq.length kvp.Value = 0)
    |> Seq.map (fun kvp -> kvp.Key)
    |> Seq.sort
    |> Seq.tryHead

let rec findSol cache orderAcc timeAcc worker1 worker2 =
    match tryGetNext cache with
    | None -> acc
    | Some ch -> printfn("Found item")
                 findSol (cache |> Map.remove ch |> Map.map (fun c deps -> Set.remove ch deps)) (acc + (string ch))

let result1 = findSol cache "" 0 newWorker newWorker
