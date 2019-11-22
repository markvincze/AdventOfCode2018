open System.IO
open System.Text.RegularExpressions
open System

type Coordinate = int * int * int

type Nanobot = {
    Position : Coordinate
    Range : int
}

type OctantLocation = {
    Corner : Coordinate
    Width : Coordinate
}

type Octant = {
    Location : OctantLocation
    InsideCount : int
    InRangeCount : int
}

// pos=<30268215,-1711562,83940876>, r=91888282
let botRegex = Regex("pos=<(-?\\d*),(-?\\d*),(-?\\d*)>, r=(\\d*)", RegexOptions.Compiled)

let parse line = let rm = botRegex.Match line
                 let group (i : int) = (rm.Groups.[i].Value) |> Int32.Parse

                 {
                     Position = (group 1, group 2, group 3)
                     Range = group 4
                 }

let bots = File.ReadAllLines("FSharp/23-emergency-teleportation-input.txt")
           |> Array.map parse

let strongest = bots
                |> Array.maxBy (fun b -> b.Range)

let dist (x1, y1, z1) (x2, y2, z2) =
    (abs (x1 - x2)) + (abs (y1 - y2)) + (abs (z1 - z2))

let result = bots
             |> Array.filter (fun b -> (dist b.Position strongest.Position) <= strongest.Range)
             |> Array.length

let minX = bots |> Array.map ((fun b -> b.Position) >> (fun (x, y, z) -> x)) |> Array.min
let minY = bots |> Array.map ((fun b -> b.Position) >> (fun (x, y, z) -> y)) |> Array.min
let minZ = bots |> Array.map ((fun b -> b.Position) >> (fun (x, y, z) -> z)) |> Array.min

let maxX = bots |> Array.map ((fun b -> b.Position) >> (fun (x, y, z) -> x)) |> Array.max
let maxY = bots |> Array.map ((fun b -> b.Position) >> (fun (x, y, z) -> y)) |> Array.max
let maxZ = bots |> Array.map ((fun b -> b.Position) >> (fun (x, y, z) -> z)) |> Array.max

let width = maxX - minX
let height = maxY - minY
let depth = maxZ - minZ

let cubeSize = width * height * depth

let minRange = bots |> Array.map (fun b -> b.Range) |> Array.min

let isInside octant (x, y, z) =
    let (cx, cy, cz) = octant.Corner
    let (wx, wy, wz) = octant.Width
    x >= cx && x <= cx + wx &&
    y >= cy && y <= cy + wy &&
    z >= cz && z <= cz + wz

let closest x (x1, x2) =
    if x < x1 
    then x1
    else if x > x2
         then x2
         else x

let closestPoint octantLoc bot =
    let botX, botY, botZ = bot.Position
    let cornerX, cornerY, cornerZ = bot.Position
    let widthX, widthY, widthZ = bot.Position
    (closest botX (cornerX, (cornerX + widthX)),
     closest botY (cornerY, (cornerY + widthY)),
     closest botZ (cornerZ, (cornerZ + widthZ)))

let isInRange octantLoc bot =
    (dist (closestPoint octantLoc bot) bot.Position) <= bot.Range

let botsInside octant bots =
    bots
    |> Array.map (fun b -> b.Position)
    |> Array.filter (isInside octant)
    |> Array.length

let botsInRange octant bots =
    bots
    |> Array.filter (isInRange octant)
    |> Array.length

let octant corner width bots =
    let octantLocation = {
        Corner = corner
        Width = width
    }
    {
        Location = octantLocation
        InsideCount = botsInside octantLocation bots
        InRangeCount = botsInRange octantLocation bots
    }

let fullOctant = octant (minX, minY, minZ) (width, height, depth) bots

let split octantLoc =
    let cx, cy, cz = octantLoc.Corner
    let wx, wy, wz = octantLoc.Width
    let widthXHalf1 = wx / 2
    let widthXHalf2 = if wx % 2 = 0 then wx / 2 else wx / 2 + 1
    let widthYHalf1 = wy / 2
    let widthYHalf2 = if wy % 2 = 0 then wy / 2 else wy / 2 + 1
    let widthZHalf1 = wz / 2
    let widthZHalf2 = if wz % 2 = 0 then wz / 2 else wz / 2 + 1

    [
        { Corner = (cx, cy, cz); Width = (widthXHalf1, widthYHalf1, widthZHalf1) }
        { Corner = (cx + widthXHalf1, cy, cz); Width = (widthXHalf2, widthYHalf1, widthZHalf1) }
        { Corner = (cx, cy + widthYHalf1, cz); Width = (widthXHalf1, widthYHalf2, widthZHalf1) }
        { Corner = (cx, cy, cz + widthZHalf1); Width = (widthXHalf1, widthYHalf1, widthZHalf2) }
        { Corner = (cx + widthXHalf1, cy + widthYHalf1, cz); Width = (widthXHalf2, widthYHalf2, widthZHalf1) }
        { Corner = (cx + widthXHalf1, cy, cz + widthZHalf1); Width = (widthXHalf2, widthYHalf1, widthZHalf2) }
        { Corner = (cx, cy + widthYHalf1, cz + widthZHalf1); Width = (widthXHalf1, widthYHalf2, widthZHalf2) }
        { Corner = (cx + widthXHalf1, cy + widthYHalf1, cz + widthZHalf1); Width = (widthXHalf2, widthYHalf2, widthZHalf2) }
    ]