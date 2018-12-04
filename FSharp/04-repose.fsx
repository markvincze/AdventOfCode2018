open System
open System.IO
open System.Text.RegularExpressions

type LogEntry =
| BeginShift of int
| FallsAsleep
| WakesUp

type LogLine = {
    Timestamp : DateTime
    Entry : LogEntry
}

// Examples
// [1518-10-03 00:47] falls asleep
// [1518-07-26 23:50] Guard #487 begins shift
// [1518-06-22 00:48] wakes up
let logLineRegex = new Regex("\\[(\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d)\\] (.*)", RegexOptions.Compiled)

let parseLine line = let rm = logLineRegex.Match line
                     {
                         Timestamp = rm.Groups.[1].Value |> DateTime.Parse
                         Entry = match rm.Groups.[2].Value with 
                                 | "falls asleep" -> FallsAsleep
                                 | "wakes up" -> WakesUp
                                 | s -> BeginShift (Regex.Match(s, "Guard #(\\d+) begins shift").Groups.[1].Value |> Int32.Parse)
                     }

let logLines = File.ReadAllLines("FSharp/04-repose-input.txt")
               |> Array.map parseLine
               |> Array.sortBy (fun l -> l.Timestamp)
               |> Array.toList

type GuardHour = bool array

type GuardStats = GuardHour list

type Stats = Map<int, GuardStats>

let markSleep (hour : GuardHour) from until =
    for i = from to until do
        hour.[i] <- true

let collectStats logLines =
    let rec collectStatsRec logLines stats currentHour guardId fellAsleepMinute =
        match logLines with
        | [] -> stats
        | h :: t -> match h.Entry with
                    | FallsAsleep -> collectStatsRec t stats currentHour guardId h.Timestamp.Minute
                    | WakesUp -> markSleep currentHour fellAsleepMinute (h.Timestamp.Minute - 1)
                                 collectStatsRec t stats currentHour guardId fellAsleepMinute
                    | BeginShift id -> let newStats = match Map.tryFind guardId stats with
                                                      | None -> Map.add guardId [ currentHour ] stats
                                                      | Some gs -> Map.add guardId ( currentHour :: gs ) stats
                                       collectStatsRec t newStats (Array.create 60 false) id 0

    match logLines with
    | firstEntry :: rest ->
        match firstEntry.Entry with
        | BeginShift id -> collectStatsRec rest Map.empty<int, GuardStats> (Array.create 60 false) id 0
        | _ -> failwith "Log has to start with BeginShift"
    | _ -> failwith "Not enought log lines"

let stats = collectStats logLines

let laziestGuardId, laziestGuardStats =
    stats
    |> Map.toList
    |> List.sortByDescending (fun (guardid, guardstats) -> guardstats
                                                           |> List.map (fun guardHour -> guardHour |> Array.filter id |> Array.length )
                                                           |> List.sum)
    |> List.head

let findMostSleptMinute (guardStats : GuardStats) =
    [ 0..59 ]
    |> Seq.sortByDescending (fun minute -> guardStats
                                           |> List.map (fun hour -> hour.[minute])
                                           |> List.filter id
                                           |> List.length)
    |> Seq.head
let mostSleptMinute = findMostSleptMinute laziestGuardStats

let result1 = laziestGuardId * mostSleptMinute

// Part 2
let (guardId, sleepiestMinute, sleepCount) =
    stats
    |> Map.toList
    |> List.map (fun (guardId, guardStats) -> let sleepiestMinute, sleepCount =
                                                  [ 0..59 ]
                                                  |> Seq.map (fun minute -> let sleepCount = guardStats
                                                                                             |> List.map (fun hour -> (hour.[minute]))
                                                                                             |> List.filter id
                                                                                             |> List.length
                                                                            (minute, sleepCount))
                                                  |> Seq.sortByDescending snd
                                                  |> Seq.head
                                              (guardId, sleepiestMinute, sleepCount))
    |> List.sortByDescending (fun (_, _, sleepCount) -> sleepCount)
    |> List.head

let result2 = guardId * sleepiestMinute
