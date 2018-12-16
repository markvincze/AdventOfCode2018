open System
open System.IO

let initial = "#......##...#.#.###.#.##..##.#.....##....#.#.##.##.#..#.##........####.###.###.##..#....#...###.##"
              |> Seq.map (fun c -> c = '#')
              |> Seq.toArray

type Rule = {
    Left2: bool
    Left1: bool
    Center: bool
    Right1: bool
    Right2: bool
    NextGen: bool
}

let parseRule (line : string) =
    {
        Left2 = line.[0] = '#'
        Left1 = line.[1] = '#'
        Center = line.[2] = '#'
        Right1 = line.[3] = '#'
        Right2 = line.[4] = '#'
        NextGen = line.[9] = '#'
    }

let rules = File.ReadAllLines("FSharp/12-subterranean-input.txt")
            |> Array.map parseRule

let paddingSize = 10000

let padding = Array.create paddingSize false

let state = Array.concat [ padding; initial; padding]

let next rules (state : bool array) =
    let matchingRule = rules
                       |> Array.find (fun r -> state.[0] = r.Left2 && state.[1] = r.Left1 && state.[2] = r.Center && state.[3] = r.Right1 && state.[4] = r.Right2)
    matchingRule.NextGen

let evolve state =
    let size = Array.length state
    [0..(size - 1)]
    |> List.map (fun i -> if i >= 2 && i <= (size - 3)
                          then next rules state.[(i - 2)..(i + 2)]
                          else false)
    |> List.toArray

let rec evolveN n (state : bool array) =
    if state.[0..5] |> Array.contains true || state.[((state |> Array.length) - 6)..((state |> Array.length) - 1)] |> Array.contains true
    then failwith (sprintf "Not enought padding at N: %d" n)
    else ()

    match n with
    | 0 -> state
    | n -> state |> evolve |> evolveN (n - 1)

let totalValue state =
    state
    |> Array.indexed
    |> Array.map (fun (i, v) -> (i - paddingSize, v))
    |> Array.filter snd
    |> Array.sumBy fst

//Part 1
let result1 = state
              |> evolveN 20
              |> totalValue

let rec loop state iter n previous =
    let state = state |> evolveN iter
    let value = state |> totalValue

    printfn "Value: %d,\tIncrease: %d" value (value - previous)

    if n > 1
    then loop state iter (n - 1) value
    else ()
