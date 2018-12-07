open System
open System.IO

let coordsOriginal = File.ReadAllLines("FSharp/06-coordinates-input.txt")
                     |> Array.map (fun line -> let split = line.Split [|','|]
                                               (split.[0] |> Int32.Parse, split.[1] |> Int32.Parse))

let count = coordsOriginal |> Array.length

let minX = coordsOriginal |> Array.map fst |> Array.min
let maxX = coordsOriginal |> Array.map fst |> Array.max
let minY = coordsOriginal |> Array.map snd |> Array.min
let maxY = coordsOriginal |> Array.map snd |> Array.max

let width = maxX - minX + 1
let height = maxY - minY + 1

let coords = coordsOriginal |> Array.map (fun (x, y) -> (x - minX, y - minY))

type Cell =
| Coord of int
| Closest of int
| Tie
| Unknown

let grid = Array2D.create width height Unknown

coords
|> Array.iteri (fun i (x, y) -> grid.[x, y] <- Coord i)

let manhattan (x1 : int, y1 : int) (x2 : int, y2 : int) = (Math.Abs (x1 - x2)) + (Math.Abs (y1 - y2))

let getClosest coords x y =
    let minDist = coords |> Array.map (fun c -> manhattan c (x, y)) |> Array.min
    let ties = coords |> Array.indexed |> Array.filter (fun (i, c) -> manhattan c (x, y) = minDist)
    if Array.length ties = 1
    then Closest (fst ties.[0])
    else Tie

let fillCell (grid : Cell[,]) x y =
    match grid.[x, y] with
    | Coord _ -> ()
    | Unknown -> grid.[x, y] <- getClosest coords x y
    | _ -> failwith("All empty cells should be Unknown.")

for x = 0 to width - 1 do
    for y = 0 to height - 1 do
        fillCell grid x y

let coordOrClosest cell = match cell with
                          | Coord i -> Some i
                          | Closest i -> Some i
                          | _ -> None

let infinites = 
    [
        [| 0..width - 1 |] |> Array.map (fun x -> grid.[x, 0] |> coordOrClosest)
        [| 0..width - 1 |] |> Array.map (fun x -> grid.[x, height - 1] |> coordOrClosest)
        [| 0..height - 1 |] |> Array.map (fun y -> grid.[0, y] |> coordOrClosest)
        [| 0..height - 1 |] |> Array.map (fun y -> grid.[width - 1, y] |> coordOrClosest)
    ]
    |> Array.concat
    |> Array.filter Option.isSome
    |> Array.map Option.get
    |> Array.distinct

let result1 =
    [|0..count-1|]
    |> Array.except infinites
    |> Array.map (fun i -> [ for x = 0 to width - 1 do
                               for y = 0 to height - 1 do
                                 yield grid.[x, y] ]
                           |> List.filter (fun cell -> match cell with
                                                        | Coord x -> x = i
                                                        | Closest x -> x = i
                                                        | Tie -> false
                                                        | Unknown -> failwith("Unknown cell"))
                           |> List.length)
    |> Array.max

// Part 2
let totalDistances =
    [ for x = 0 to width - 1 do
        for y = 0 to height - 1 do
          yield (x, y) ]
    |> List.map (fun c1 -> coords
                           |> Array.sumBy (fun c2 -> manhattan c1 c2))

let result2 = totalDistances |> List.filter (fun d -> d < 10000) |> List.length