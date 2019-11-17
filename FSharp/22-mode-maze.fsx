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

let depth = 11541
let target = (14, 778)
// let depth = 510
// let target = (10, 10)

// let geoIndices = Array2D.create ((fst target) + 10) ((snd target) + 10) 0
let geoIndices = Array2D.create ((fst target) * 3 + 1) ((snd target) * 3 + 1) -1

let erosionLevel geoIndex = (geoIndex + depth) % 20183

let rec geoIndex (geoIndices : int [,]) (x, y) =
    match geoIndices.[x, y] with
    | -1 -> let r = match (x, y) with
                    | (0, 0) -> 0
                    | (x, y) when (x, y) = target -> 0
                    | (x, 0) -> x * 16807
                    | (0, y) -> y * 48271
                    | (x, y) -> (geoIndex geoIndices (x - 1, y) |> erosionLevel) * (geoIndex geoIndices (x, y - 1) |> erosionLevel)
            geoIndices.[x, y] <- r
            r
    | i -> i

let regionType erosionLevel = match erosionLevel % 3 with
                              | 0 -> Rocky
                              | 1 -> Wet
                              | 2 -> Narrow

let riskLevel region = match region with
                       | Rocky -> 0
                       | Wet -> 1
                       | Narrow -> 2

let (targetX, targetY) = target

let iterCount = (min (fst target) (snd target)) * 3

for i in 0..iterCount do
    for x in i..targetX*3 do
        geoIndices.[x, i] <- (geoIndex geoIndices (x, i))

    for y in i..targetY*3 do
        geoIndices.[i, y] <- (geoIndex geoIndices (i, y))

let result = seq { for x in 0..targetX do
                       for y in 0..targetY do
                           yield (x, y) }
             |> Seq.sumBy (fun (x, y) -> geoIndices.[x, y] |> erosionLevel |> regionType |> riskLevel)

let print () =
    for y in 0..targetY*3 do
        for x in 0..targetX*3 do
            let regionType = geoIndices.[x, y] |> erosionLevel |> regionType
            let char = match (x, y), regionType with
                       | (x, y), _ when (x, y) = target -> 'T'
                       | _, Rocky -> '.'
                       | _, Wet -> '='
                       | _, Narrow -> '|'
            printf "%c" char
        printfn ""


// Part 2

type ShortestDistances = {
    WithClimbingGear : int option
    WithTorch : int option
    WithNeither : int option
}

let shortestDistancesCache = Map.empty<int*int,ShortestDistances> |> Map.add (0, 0) { WithClimbingGear = Some 8; WithTorch = Some 0; WithNeither = Some 8 }

let queue = (0, 0) |> enqueue emptyQueue

let getMin shortestDist = [shortestDist.WithClimbingGear; shortestDist.WithTorch; shortestDist.WithNeither]
                          |> List.filter Option.isSome
                          |> List.map Option.get
                          |> List.min

type Equipment = ClimbingGear | Torch | Neither

let allowedEquipments region = match region with
                               | Rocky -> [ ClimbingGear; Torch ]
                               | Wet -> [ ClimbingGear; Neither ]
                               | Narrow -> [ Torch; Neither ]

let shortest equipment shortestDistances = match equipment with
                                           | ClimbingGear -> shortestDistances.WithClimbingGear
                                           | Torch -> shortestDistances.WithTorch
                                           | Neither -> shortestDistances.WithNeither

let rec discover shortestDistancesCache queue (minDistInQueue : int) (distToTarget : int option) =
    if queue = emptyQueue
    then match distToTarget with
         | Some dist -> dist, minDistInQueue
         | None -> failwith "Didn't determine the distance, but the queue is empty."
                   0, 0
    else let (x, y), queue = dequeue queue
         let step queue shortestDistancesCache position minDistInQueue distToTarget =
             let sx, sy = position
             if sx < 0 || sy < 0 || sx >= targetX*3 || sy >= targetY*3
             then queue, shortestDistancesCache, minDistInQueue, distToTarget
             else let fromRegionType = geoIndex geoIndices (x, y) |> erosionLevel |> regionType
                  let stepRegionType = geoIndex geoIndices (sx, sy) |> erosionLevel |> regionType
                  let shortestDistances = Map.find (x, y) shortestDistancesCache
                  let stepShortestDistances = Map.tryFind position shortestDistancesCache

                  let fromDists = allowedEquipments fromRegionType
                                  |> List.map (fun eq -> (eq, shortest eq shortestDistances))
                                  |> Map.ofList

                  let from = [ ClimbingGear; Torch ]
                  let where = [ ClimbingGear; Neither ]
                  let where = [ Torch; Neither ]

                  let climbingGear = match shortestDistances.WithClimbingGear with
                                     | Some v -> v + 1
                                     | None -> (min (Option.get shortestDistances.WithTorch) (Option.get shortestDistances.WithNeither)) + 7 + 1
                  let torch = match shortestDistances.WithTorch with
                              | Some v -> v + 1
                              | None -> (min (Option.get shortestDistances.WithClimbingGear) (Option.get shortestDistances.WithNeither)) + 7 + 1
                  let neither = match shortestDistances.WithNeither with
                                | Some v -> v + 1
                                | None -> (min (Option.get shortestDistances.WithClimbingGear) (Option.get shortestDistances.WithTorch)) + 7 + 1
                  let newShortestDistances =
                      match stepRegionType with
                      | Rocky -> match stepShortestDistances with
                                 | None -> Some { WithNeither = None; WithClimbingGear = Some climbingGear; WithTorch = Some torch }
                                 | Some sd -> if (Option.get sd.WithClimbingGear) <= climbingGear && (Option.get sd.WithTorch) <= torch
                                              then None
                                              else Some { WithNeither = None;
                                                          WithClimbingGear = Some (min (Option.get sd.WithClimbingGear) climbingGear);
                                                          WithTorch = Some (min (Option.get sd.WithTorch) torch) }
                      | Wet -> match stepShortestDistances with
                               | None -> Some { WithNeither = Some neither; WithClimbingGear = Some climbingGear; WithTorch = None }
                               | Some sd -> if (Option.get sd.WithClimbingGear) <= climbingGear && (Option.get sd.WithNeither) <= neither
                                            then None
                                            else Some { WithNeither = Some (min (Option.get sd.WithNeither) neither);
                                                        WithClimbingGear = Some (min (Option.get sd.WithClimbingGear) climbingGear);
                                                        WithTorch = None }
                      | Narrow -> match stepShortestDistances with
                                  | None -> Some { WithNeither = Some neither; WithClimbingGear = None; WithTorch = Some torch }
                                  | Some sd -> if (Option.get sd.WithTorch) <= torch && (Option.get sd.WithNeither) <= neither
                                               then None
                                               else Some { WithNeither = Some (min (Option.get sd.WithNeither) neither);
                                                           WithClimbingGear = None;
                                                           WithTorch = Some (min (Option.get sd.WithTorch) torch) }
                  match newShortestDistances with
                  | None -> queue, shortestDistancesCache, minDistInQueue, distToTarget
                  | Some sd -> let distToTarget = if position = target
                                                  then match distToTarget with
                                                       | None -> sd.WithTorch
                                                       | Some d -> Some (min (Option.get sd.WithTorch) d)
                                                  else distToTarget
                               let queue = match distToTarget with
                                           | Some d -> if d > (getMin sd)
                                                       then enqueue queue position
                                                       else queue
                                           | None -> enqueue queue position
                               queue, Map.add position sd shortestDistancesCache, (min minDistInQueue (getMin sd)), distToTarget

         let queue, shortestDistancesCache, minDistInQueue, distToTarget = step queue shortestDistancesCache (x+1, y) minDistInQueue distToTarget
         let queue, shortestDistancesCache, minDistInQueue, distToTarget = step queue shortestDistancesCache (x, y+1) minDistInQueue distToTarget
         let queue, shortestDistancesCache, minDistInQueue, distToTarget = step queue shortestDistancesCache (x-1, y) minDistInQueue distToTarget
         let queue, shortestDistancesCache, minDistInQueue, distToTarget = step queue shortestDistancesCache (x, y-1) minDistInQueue distToTarget

        //  match distToTarget with
        //  | None -> ()
        //  | Some d -> if d <= minDistInQueue
        //              then printfn "Shortest to target: %A, in q: %A" d minDistInQueue
        //                   failwith "alma"
        //              else ()
        //  match distToTarget with
        //  | None -> ()
        //  | Some d -> let minDistInQueue = if queue = emptyQueue
        //                                   then 0
        //                                   else allItems queue
        //                                        |> Seq.map ((fun p -> Map.find p shortestDistancesCache) >> getMin)
        //                                        |> Seq.min
        //              printfn "distToTarget: %d, minDistInQueue: %d" d minDistInQueue
        //              if d <= minDistInQueue
        //              then printfn "Shortest to target: %A, Min dist in queue: %d" d minDistInQueue
        //                   failwith "alma"
        //              else ()

        //  if (x, y) = target && (Option.get shortestDistances.WithTorch) <= minDist
        //  then printfn "Shortest to target: %A" shortestDistances
        //       failwith "alma"
        //  else ()

         discover shortestDistancesCache queue minDistInQueue distToTarget

// discover shortestDistancesCache queue 0 None
