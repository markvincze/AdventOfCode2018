open System
open System.Text.RegularExpressions
open System.IO
open System.Linq.Expressions

type Block =
| Horizontal of (int * (int * int))
| Vertical of (int * (int * int))

// x=483, y=786..801
// y=1171, x=469..486
let regex = new Regex("(\\w)=(\\d+), \\w=(\\d+)\\.\\.(\\d+)")
let parseLine line =
    let rm = line |> regex.Match

    let iv (i : int) = rm.Groups.[i].Value |> Int32.Parse

    let c = rm.Groups.[1].Value

    match c with
    | "x" -> Vertical (iv 2, (iv 3, iv 4))
    | "y" -> Horizontal (iv 2, (iv 3, iv 4))
    | _ -> failwith "Invalid input"

let blocks = File.ReadAllLines("17-reservoir-input.txt")
             |> Array.map parseLine

let minX = blocks |> Array.map (fun b -> match b with
                                         | Horizontal (_, (x1, _)) -> x1
                                         | Vertical (x, (_, _)) -> x)
                  |> Array.min

let maxX = blocks |> Array.map (fun b -> match b with
                                         | Horizontal (_, (_, x2)) -> x2
                                         | Vertical (x, (_, _)) -> x)
                  |> Array.max

let minY = blocks |> Array.map (fun b -> match b with
                                         | Horizontal (y, (_, _)) -> y
                                         | Vertical (_, (y1, _)) -> y1)
                  |> Array.min

let maxY = blocks |> Array.map (fun b -> match b with
                                         | Horizontal (y, (_, _)) -> y
                                         | Vertical (_, (_, y2)) -> y2)
                  |> Array.max

type Tile =
| Sand
| Clay
| WaterRest
| WaterFlow

let map = Array2D.create (maxX - minX + 3) (maxY + 1) Sand

blocks
|> Array.iter (fun b -> match b with
                        | Horizontal (y, (x1, x2)) -> [ x1..x2 ] |> List.iter (fun x -> map.[x - minX + 1, y] <- Clay)
                        | Vertical (x, (y1, y2)) -> [ y1..y2 ] |> List.iter (fun y -> map.[x - minX + 1, y] <- Clay))

let printMap map =
    for y = 0 to (Array2D.length2 map) - 1 do
        for x = 0 to (Array2D.length1 map) - 1 do
            let c = match map.[x, y] with
                    | Sand -> '.'
                    | Clay -> '#'
                    | WaterRest -> '~'
                    | WaterFlow -> '|'
            printf "%c" c
        printfn ""

let springPos = (501 - minX, minY)

let right (x, y) = x + 1, y
let down (x, y) = x, y + 1
let left (x, y) = x - 1, y

let at (x, y) =
    if x < 0 || y < 0 || x >= (Array2D.length1 map) || y >= (Array2D.length2 map)
    then Sand
    else map.[x, y]
let set (x, y) tile = //printfn "called set with %A" (x, y)
                      map.[x, y] <- tile

let isBottom (_, y) = y >= ((Array2D.length2 map) - 1)

type FlowDownResult =
| PouredDown of int
| HitFloor of int

type FlowSideResult =
| PouredOffSide of int
| HitWall of int

let inc value res = match res with
                    | PouredDown x -> PouredDown (x + value)
                    | HitFloor x -> HitFloor (x + value)

let incSide value res = match res with
                        | PouredOffSide x -> PouredOffSide (x + value)
                        | HitWall x -> HitWall (x + value)

let rec flowSide pos dir =
    set pos WaterFlow
    match (down pos |> at, dir pos |> at) with
    | Sand, _ -> match flowDown (down pos) with
                 | PouredDown x -> PouredOffSide (x + 1)
                 | HitFloor x -> flowSide (dir pos) dir |> incSide x
    | _, Clay -> HitWall 1
    | _, WaterFlow -> PouredOffSide 1
    | _, Sand -> flowSide (dir pos) dir |> incSide 1
    | x -> failwith (sprintf "Invalid state underneath when flowing to the side: %A" x)

and setUntilWall pos dir tile =
    set pos tile
    match dir pos |> at with
    | Clay -> ()
    | _ -> setUntilWall (dir pos) dir tile

and flowSides pos = 
    let resultLeft = flowSide pos left
    let resultRight = flowSide pos right
    // let resultRight = HitWall 0

    // printfn "%A: Result in flowSides: Left: %A, Right: %A" pos resultLeft resultRight

    match resultLeft, resultRight with
    | HitWall v1, HitWall v2 -> setUntilWall pos left WaterRest
                                setUntilWall pos right WaterRest
                                set pos WaterRest
                                HitFloor (v1 + v2 - 1)
    | PouredOffSide v1, HitWall v2 -> PouredDown (v1 + v2 - 1)
    | HitWall v1, PouredOffSide v2 -> PouredDown (v1 + v2 - 1)
    | PouredOffSide v1, PouredOffSide v2 -> PouredDown (v1 + v2 - 1)

and flowDown pos =
    // printfn "flowDown called with %A" pos
    set pos WaterFlow
    if pos |> isBottom
    then PouredDown 1
    else match down pos |> at with
         | Sand -> match flowDown (down pos) with
                   | PouredDown x -> //printfn "%A: Partial result: %d" pos x
                                     PouredDown (x + 1)
                   | HitFloor x -> //printfn "%A: Partial result: %d" pos x
                                   (flowSides pos |> inc x)
         | Clay -> flowSides pos
         | WaterRest -> flowSides pos
        //  | WaterRest -> failwithf "We poured down on resting water (%A)" pos
         | WaterFlow -> PouredDown 1

// printMap map

[<EntryPoint>]
let main argv =
    // printMap map
    let result1 = try flowDown springPos
                  with
                     | Failure msg -> printfn "Failure: %s" msg
                                      (PouredDown 1)
                     |  ex -> printfn "Exception: %s" ex.Message
                              (PouredDown 1)

    // printMap map

    match result1 with
    | PouredDown x -> printfn "Part 1 result: %d" x
    | _ -> failwithf "Invalid result: %A" result1

    let res1 = File.ReadAllText "endMap.txt"
               |> Seq.filter (fun c -> c = '|' || c = '~')
               |> Seq.length
    let res2 = File.ReadAllText "endMap.txt"
               |> Seq.filter (fun c -> c = '~')
               |> Seq.length
    printfn "Part 1 result: %d, Part 2 result: %d" res1 res2
    0
