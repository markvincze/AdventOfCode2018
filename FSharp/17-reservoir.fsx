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

let blocks = File.ReadAllLines("FSharp/17-reservoir-input.txt")
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
    printfn ""

let springPos = (501 - minX, minY)

let right (x, y) = x + 1, y
let down (x, y) = x, y + 1
let left (x, y) = x - 1, y

let at (x, y) = map.[x, y]
let set (x, y) tile = map.[x, y] <- tile

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

let incSide res = match res with
                  | PouredOffSide x -> PouredOffSide (x + 1)
                  | HitWall x -> HitWall (x + 1)

let rec flowSide pos dir =
    set pos WaterFlow
    match (down pos |> at, dir pos |> at) with
    | Sand, _ -> match flowDown (down pos) with
                 | PouredDown x -> PouredOffSide (x + 1)
                 | HitFloor x -> flowSide (dir pos) dir |> incSide
    | _, Clay -> HitWall 1
    | _, WaterFlow -> PouredOffSide 1
    | _, Sand -> flowSide (dir pos) dir |> incSide
    | x -> failwith (sprintf "Invalid state underneath when flowing to the side: %A" x)

and setUntilWall pos dir tile =
    set pos tile
    match dir pos |> at with
    | Clay -> ()
    | _ -> setUntilWall (dir pos) dir tile

and flowSides pos = 
    let resultLeft = flowSide pos left
    let resultRight = flowSide pos right

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
                   | PouredDown x -> PouredDown (x + 1)
                   | HitFloor x -> (flowSides pos |> inc x)
         | Clay | WaterRest -> flowSides pos
         | WaterFlow -> PouredDown 1

let result1 = flowDown springPos

// printMap map