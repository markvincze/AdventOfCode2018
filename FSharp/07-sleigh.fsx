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

let progressWork cache worker =
    match worker.Job with
    | Some ch -> match worker.WorkLeft with
                 | 1 -> newWorker, cache |> Map.remove ch |> Map.map (fun c deps -> Set.remove ch deps)
                 | _ -> { worker with WorkLeft = worker.WorkLeft - 1 }, cache
    | None -> worker, cache

let workHours ch = int ch - int 'A' + 61

let tryPickJob (cache : Map<char, Set<char>>) allWorkers worker  =
    match worker.Job with
    | Some _ -> worker
    | None -> let picked = cache
                           |> Seq.filter (fun kvp -> Seq.length kvp.Value = 0)
                           |> Seq.filter (fun kvp -> not (List.exists (fun w -> w.Job = Some kvp.Key) allWorkers))
                           |> Seq.map (fun kvp -> kvp.Key)
                           |> Seq.sort
                           |> Seq.tryHead
              match picked with
              | Some p -> { Job = Some p; WorkLeft = workHours p}
              | None -> worker

let rec findSol cache orderAcc timeAcc (workers : Worker list) =
    let workers, cache =
        List.fold
          (fun (workers, cache) worker -> let worker, cache = progressWork cache worker
                                          worker :: workers, cache)
          ([], cache)
          workers

    let workers = workers |> List.fold (fun l w -> tryPickJob cache (List.append l workers) w :: l) []

    if workers |> List.forall (fun w -> w.Job = None)
    then (orderAcc, timeAcc)
    else findSol cache "" (timeAcc + 1) workers

let result1 = findSol cache "" 0 (List.init 5 (fun _ -> newWorker))
