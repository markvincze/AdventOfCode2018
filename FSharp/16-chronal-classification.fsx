open System
open System.IO
open System.Text.RegularExpressions

type Op =
| Addr
| Addi
| Mulr
| Muli
| Banr
| Bani
| Borr
| Bori
| Setr
| Seti
| Gtir
| Gtri
| Gtrr
| Eqir
| Eqri
| Eqrr

let allOps = [| Addr; Addi; Mulr; Muli; Banr; Bani; Borr; Bori; Setr; Seti; Gtir; Gtri; Gtrr; Eqir; Eqri; Eqrr |]

type Reading = {
    RegsBefore : int array
    RegsAfter : int array
    Instruction : int array
}

// Format:
// Before: [1, 2, 2, 1]
// 9 3 1 3
// After:  [1, 2, 2, 1]
let parseReading (lines : string array) =
    let rm1 = Regex.Match(lines.[0], "Before: \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]")
    let rm2 = Regex.Match(lines.[1], "(\\d+) (\\d+) (\\d+) (\\d+)")
    let rm3 = Regex.Match(lines.[2], "After:  \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]")

    let getV (rm : Match) (index : int) = rm.Groups.[index].Value |> Int32.Parse

    {
        RegsBefore = [| getV rm1 1; getV rm1 2; getV rm1 3; getV rm1 4;|]
        Instruction = [| getV rm2 1; getV rm2 2; getV rm2 3; getV rm2 4;|]
        RegsAfter = [| getV rm3 1; getV rm3 2; getV rm3 3; getV rm3 4;|]
    }

let isMatch r op =
    let output = r.RegsAfter.[r.Instruction.[3]]
    let val1 = r.Instruction.[1]
    let val2 = r.Instruction.[2]
    let regVal1 = r.RegsBefore.[r.Instruction.[1]]
    let regVal2 = r.RegsBefore.[r.Instruction.[2]]
    match op with
    | Addr -> output = regVal1 + regVal2
    | Addi -> output = regVal1 + val2
    | Mulr -> output = regVal1 * regVal2
    | Muli -> output = regVal1 * val2
    | Banr -> output = (regVal1 &&& regVal2)
    | Bani -> output = (regVal1 &&& val2)
    | Borr -> output = (regVal1 ||| regVal2)
    | Bori -> output = (regVal1 ||| val2)
    | Setr -> output = regVal1
    | Seti -> output = val1
    | Gtir -> (output = 1 && val1 > regVal2) || (output = 0 && val1 <= regVal2)
    | Gtri -> (output = 1 && regVal1 > val2) || (output = 0 && regVal1 <= val2)
    | Gtrr -> (output = 1 && regVal1 > regVal2) || (output = 0 && regVal1 <= regVal2)
    | Eqir -> (output = 1 && val1 = regVal2) || (output = 0 && val1 <> regVal2)
    | Eqri -> (output = 1 && regVal1 = val2) || (output = 0 && regVal1 <> val2)
    | Eqrr -> (output = 1 && regVal1 = regVal2) || (output = 0 && regVal1 <> regVal2)

let readings = File.ReadAllLines("FSharp/16-chronal-classification-input.txt")
               |> Array.chunkBySize 4
               |> Array.filter (fun g -> g.[0].StartsWith "Before")
               |> Array.map parseReading

let result1 = readings
              |> Array.filter (fun r -> (allOps |> Array.filter (isMatch r) |> Array.length) >= 3)
              |> Array.length

let addReading map reading =
    let matchingOps = allOps |> Array.filter (isMatch reading) |> Set.ofArray
    let ops = Map.tryFind reading.Instruction.[0] map
    match ops with
    | None -> Map.add reading.Instruction.[0] matchingOps map
    | Some ops -> Map.add reading.Instruction.[0] (Set.intersect ops matchingOps) map

let matchingOps =
    readings |> Array.fold addReading Map.empty<int, Set<Op>>

let buildOpIds matchingOps =
    let rec buildOpIdsRec (acc : Map<int, Op>) (matchingOps : Map<int, Set<Op>>) =
        if Map.count acc = Array.length allOps
        then acc
        else let newAcc = matchingOps |> Map.filter (fun _ ops -> Set.count ops = 1) |> Map.fold (fun (acc : Map<int, Op>) i ops -> Map.add i (Set.minElement ops) acc) acc
             let identifiedOps = matchingOps |> Map.filter (fun _ ops -> Set.count ops = 1) |> Map.toList |> List.map (fun (_, ops) -> Seq.head ops)
             let newMatchingOps = matchingOps
                                  |> Map.filter (fun _ ops -> Set.count ops > 1)
                                  |> Map.fold (fun (map : Map<int, Set<Op>>) i ops -> Map.add i (ops |> Set.filter (fun o -> not <| List.contains o identifiedOps)) map) Map.empty<int, Set<Op>>

             buildOpIdsRec newAcc newMatchingOps
    
    buildOpIdsRec Map.empty<int, Op> matchingOps

let opIds = buildOpIds matchingOps

let instructions = File.ReadAllLines("FSharp/16-chronal-classification-input2.txt")
                 |> Array.map (fun line -> line.Split(' ') |> Array.map Int32.Parse)

let state = [| 0; 0; 0; 0 |]

let execute (state : int array) (instruction : int array) =
    let op = Map.find instruction.[0] opIds
    let val1 = instruction.[1]
    let val2 = instruction.[2]
    let regVal1 = state.[instruction.[1]]
    let regVal2 = state.[instruction.[2]]
    let output = match op with
                 | Addr -> regVal1 + regVal2
                 | Addi -> regVal1 + val2
                 | Mulr -> regVal1 * regVal2
                 | Muli -> regVal1 * val2
                 | Banr -> (regVal1 &&& regVal2)
                 | Bani -> (regVal1 &&& val2)
                 | Borr -> (regVal1 ||| regVal2)
                 | Bori -> (regVal1 ||| val2)
                 | Setr -> regVal1
                 | Seti -> val1
                 | Gtir -> if val1 > regVal2 then 1 else 0
                 | Gtri -> if regVal1 > val2 then 1 else 0
                 | Gtrr -> if regVal1 > regVal2 then 1 else 0
                 | Eqir -> if val1 = regVal2 then 1 else 0
                 | Eqri -> if regVal1 = val2 then 1 else 0
                 | Eqrr -> if regVal1 = regVal2 then 1 else 0
    state.[instruction.[3]] <- output

Array.iter (execute state) instructions

state
