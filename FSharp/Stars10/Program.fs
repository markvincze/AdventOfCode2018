open System
open System.IO
open System.Text.RegularExpressions

type Point = {
    X : int
    Y : int
    XV : int
    YV : int
}

// position=<-54530, -54537> velocity=< 5,  5>
let lineRegex = new Regex("position=<([- ]\\d+), ([- ]\\d+)> velocity=<([- ]\\d+), ([- ]\\d+)>")

let parseLine line =
    let rm = line |> lineRegex.Match
    {
        X = rm.Groups.[1].Value |> Int32.Parse
        Y = rm.Groups.[2].Value |> Int32.Parse
        XV = rm.Groups.[3].Value |> Int32.Parse
        YV = rm.Groups.[4].Value |> Int32.Parse
    }

let points = File.ReadAllLines("10-stars-input.txt")
             |> Array.map parseLine

let step point = {
    X = point.X + point.XV
    Y = point.Y + point.YV
    XV = point.XV
    YV = point.YV
}

let rec progress points stepLimit currentStep minYDist minStep =
    let points = points |> Array.map step

    let yDist = (points |> Array.maxBy(fun p -> p.Y)).Y - (points |> Array.minBy(fun p -> p.Y)).Y

    let minYDist, minStep =
        if yDist < minYDist
        then yDist, currentStep
        else minYDist, minStep

    if currentStep < stepLimit
    then progress points stepLimit (currentStep + 1) minYDist minStep
    else (minYDist, minStep, points)

// let mindist, minstep, _ = progress points 100000 0 System.Int32.MaxValue 0;;
let _, _, finalPoints = progress points 10945 0 Int32.MaxValue 0

let orderedPoints = finalPoints |> Array.sortBy (fun p -> p.Y, p.X)

let minX = (orderedPoints |> Array.sortBy (fun p -> p.X)).[0].X
let minY = (orderedPoints |> Array.sortBy (fun p -> p.Y)).[0].Y

let translatedPoints = orderedPoints |> Array.map (fun p -> { X = (p.X - minX); Y = (p.Y - minY); XV = 0; YV = 0 })

let rec consolePrint points =
    match points with
    | [] -> ()
    | h :: t -> Console.SetCursorPosition (h.X, h.Y)
                Console.Write "*"
                consolePrint t

[<EntryPoint>]
let main argv =
    Console.Clear ()
    consolePrint (translatedPoints |> Array.toList)
    0
