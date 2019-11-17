type queue<'a> =
    | Queue of 'a list * 'a list

let emptyQueue = Queue([], [])

let enqueue q e = 
    match q with
    | Queue(fs, bs) -> Queue(e :: fs, bs)

let dequeue q = 
    match q with
    | Queue([], []) -> failwith "Empty queue!"
    | Queue(fs, b :: bs) -> b, Queue(fs, bs)
    | Queue(fs, []) -> 
        let bs = List.rev fs
        bs.Head, Queue([], bs.Tail)

let allItems q = match q with
                 | Queue(fs, bs) -> Seq.concat [ fs; bs ]

type RegionType =
| Rocky
| Wet
| Narrow

// let depth = 11541
// let target = (14, 778)
let depth = 510
let target = (10, 10)

// let geoIndices = Array2D.create ((fst target) + 10) ((snd target) + 10) 0
let geoIndices = Array2D.create ((fst target) * 2) ((snd target) * 2) -1

let erosionLevel geoIndex = (geoIndex + depth) % 20183

let geoIndex (geoIndices : int [,]) (x, y) =
    match (x, y) with
    | (0, 0) -> 0
    | (x, y) when (x, y) = target -> 0
    | (x, 0) -> x * 16807
    | (0, y) -> y * 48271
    | (x, y) -> (geoIndices.[x - 1, y] |> erosionLevel) * (geoIndices.[x, y - 1] |> erosionLevel)

let regionType erosionLevel = match erosionLevel % 3 with
                              | 0 -> Rocky
                              | 1 -> Wet
                              | 2 -> Narrow

let riskLevel region = match region with
                       | Rocky -> 0
                       | Wet -> 1
                       | Narrow -> 2

let (targetX, targetY) = target

let iterCount = min (fst target) (snd target)

for i in 0..iterCount do
    for x in i..targetX do
        geoIndices.[x, i] <- (geoIndex geoIndices (x, i))

    for y in i..targetY do
        geoIndices.[i, y] <- (geoIndex geoIndices (i, y))


let result = seq { for x in 0..targetX do
                       for y in 0..targetY do
                           yield (x, y) }
             |> Seq.sumBy (fun (x, y) -> geoIndices.[x, y] |> erosionLevel |> regionType |> riskLevel)

let print () =
    for y in 0..targetY do
        for x in 0..targetX do
            let regionType = geoIndices.[x, y] |> erosionLevel |> regionType
            let char = match regionType with
                       | Rocky -> '.'
                       | Wet -> '='
                       | Narrow -> '|'
            printf "%c" char
        printfn ""


// Part 2

type ShortestDistances = {
    WithClimbingGear : int option
    WithTorch : int option
    WithNeither : int option
}

let shortestDistancesCache = Map.empty<int*int,ShortestDistances> |> Map.add (0, 0) { WithClimbingGear = Some 7; WithTorch = Some 0; WithNeither = Some 7 }

let queue = (0, 0) |> enqueue emptyQueue

let rec discover shortestDistancesCache queue (distToTarget : int option) =
    if queue = emptyQueue
    then match distToTarget with
         | Some dist -> dist
         | None -> failwith "Didn't determine the distance, but the queue is empty."
                   0
    else let (x, y), queue = dequeue queue
         let shortestDistances = Map.find (x, y) shortestDistancesCache

         if (x, y) = target
         then printfn "Shortest to target: %A" shortestDistances
              failwith "alma"
         else ()

         let step queue shortestDistancesCache position =
             let sx, sy = position
            //  if sx < 0 || sy < 0 || sx > 20 || sy > 785
             if sx < 0 || sy < 0 || sx > 19 || sy > 19
             then queue, shortestDistancesCache
             else let stepRegionType = geoIndices.[sx, sy] |> erosionLevel |> regionType
                  let stepShortestDistances = Map.tryFind position shortestDistancesCache
                  match stepRegionType with
                  | Rocky -> let climbingGear = match shortestDistances.WithClimbingGear with
                                                | Some v -> v + 1
                                                | None -> (min (Option.get shortestDistances.WithTorch) (Option.get shortestDistances.WithNeither)) + 7 + 1
                             let torch = match shortestDistances.WithTorch with
                                         | Some v -> v + 1
                                         | None -> (min (Option.get shortestDistances.WithClimbingGear) (Option.get shortestDistances.WithNeither)) + 7 + 1
                             let newShortestDistances =
                                 match stepShortestDistances with
                                 | None -> Some { WithNeither = None; WithClimbingGear = Some climbingGear; WithTorch = Some torch }
                                 | Some sd -> if (Option.get sd.WithClimbingGear) <= climbingGear && (Option.get sd.WithTorch) <= torch
                                               then None
                                               else Some { WithNeither = None;
                                                           WithClimbingGear = Some (min (Option.get sd.WithClimbingGear) climbingGear);
                                                           WithTorch = Some (min (Option.get sd.WithTorch) torch) }
                             match newShortestDistances with
                             | None -> queue, shortestDistancesCache
                             | Some sd -> enqueue queue position, Map.add position sd shortestDistancesCache
                  | Wet -> let climbingGear = match shortestDistances.WithClimbingGear with
                                              | Some v -> v + 1
                                              | None -> (min (Option.get shortestDistances.WithTorch) (Option.get shortestDistances.WithNeither)) + 7 + 1
                           let neither = match shortestDistances.WithNeither with
                                         | Some v -> v + 1
                                         | None -> (min (Option.get shortestDistances.WithClimbingGear) (Option.get shortestDistances.WithTorch)) + 7 + 1
                           let newShortestDistances =
                               match stepShortestDistances with
                               | None -> Some { WithNeither = Some neither; WithClimbingGear = Some climbingGear; WithTorch = None }
                               | Some sd -> if (Option.get sd.WithClimbingGear) <= climbingGear && (Option.get sd.WithNeither) <= neither
                                            then None
                                            else Some { WithNeither = Some (min (Option.get sd.WithNeither) neither);
                                                        WithClimbingGear = Some (min (Option.get sd.WithClimbingGear) climbingGear);
                                                        WithTorch = None }
                           match newShortestDistances with
                           | None -> queue, shortestDistancesCache
                           | Some sd -> enqueue queue position, Map.add position sd shortestDistancesCache
                  | Narrow -> let torch = match shortestDistances.WithTorch with
                                          | Some v -> v + 1
                                          | None -> (min (Option.get shortestDistances.WithClimbingGear) (Option.get shortestDistances.WithNeither)) + 7 + 1
                              let neither = match shortestDistances.WithNeither with
                                            | Some v -> v + 1
                                            | None -> (min (Option.get shortestDistances.WithClimbingGear) (Option.get shortestDistances.WithTorch)) + 7 + 1
                              let newShortestDistances =
                                  match stepShortestDistances with
                                  | None -> Some { WithNeither = Some neither; WithClimbingGear = None; WithTorch = Some torch }
                                  | Some sd -> if (Option.get sd.WithTorch) <= torch && (Option.get sd.WithNeither) <= neither
                                               then None
                                               else Some { WithNeither = Some (min (Option.get sd.WithNeither) neither);
                                                           WithClimbingGear = None;
                                                           WithTorch = Some (min (Option.get sd.WithTorch) torch) }
                              match newShortestDistances with
                              | None -> queue, shortestDistancesCache
                              | Some sd -> enqueue queue position, Map.add position sd shortestDistancesCache

         let queue, shortestDistancesCache = step queue shortestDistancesCache (x+1, y)
         let queue, shortestDistancesCache = step queue shortestDistancesCache (x, y+1)
         let queue, shortestDistancesCache = step queue shortestDistancesCache (x-1, y)
         let queue, shortestDistancesCache = step queue shortestDistancesCache (x, y-1)

         discover shortestDistancesCache queue (Some 0)

// discover shortestDistancesCache queue None
