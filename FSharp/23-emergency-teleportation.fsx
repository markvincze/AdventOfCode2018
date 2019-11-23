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
    if x < x1 then x1
    elif x > x2 then x2
    else x

let furthest x (x1, x2) =
    let h = (x2 - x1) / 2
    if x <= x1 + h
    then x2
    else x1

let closestPoint octantLoc position =
    let botX, botY, botZ = position
    let cornerX, cornerY, cornerZ = octantLoc.Corner
    let widthX, widthY, widthZ = octantLoc.Width
    (closest botX (cornerX, (cornerX + widthX)),
     closest botY (cornerY, (cornerY + widthY)),
     closest botZ (cornerZ, (cornerZ + widthZ)))

let furthestPoint octantLoc position =
    let botX, botY, botZ = position
    let cornerX, cornerY, cornerZ = octantLoc.Corner
    let widthX, widthY, widthZ = octantLoc.Width
    (furthest botX (cornerX, (cornerX + widthX)),
     furthest botY (cornerY, (cornerY + widthY)),
     furthest botZ (cornerZ, (cornerZ + widthZ)))

let isInRange point bot =
    (dist point bot.Position) <= bot.Range

let botsInside octant bots =
    bots
    |> Array.filter (fun b -> isInside octant b.Position)
    |> Array.length

let botsInRange octantLocation bots =
    bots
    |> Array.filter (fun b -> (dist (closestPoint octantLocation b.Position) b.Position) <= b.Range)
    |> Array.length

let createOctant bots octantLocation =
    {
        Location = octantLocation
        InsideCount = botsInside octantLocation bots
        InRangeCount = botsInRange octantLocation bots
    }

let fullOctantLocation = {
    Corner = (minX, minY, minZ)
    Width = (width, height, depth)
}

let fullOctant = createOctant bots fullOctantLocation

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

let isLeaf bots octant =
    bots
    |> Array.forall (fun b -> 
                        let c = closestPoint octant.Location b.Position
                        let f = furthestPoint octant.Location b.Position
                        let i1 = isInRange c b
                        let i2 = isInRange f b
                        (i1 && i2) || (not i1 && not i2))

let distFromOctant octantLoc =
    dist (0, 0, 0) (closestPoint octantLoc (0, 0, 0))

let rec findSolution queue (solution : Octant option) =
    match queue with
    | [] -> solution
    | h :: t -> if isLeaf bots h
                then let newSolution = match solution with
                                       | None -> Some h
                                       | Some o when (botsInRange h.Location bots) > o.InRangeCount -> Some h
                                       | Some o when (botsInRange h.Location bots) = o.InRangeCount && (distFromOctant h.Location) < (distFromOctant o.Location) -> Some h
                                       | _ -> solution
                     findSolution t newSolution
                else if Option.isNone solution || botsInRange h.Location bots > (Option.get solution).InRangeCount
                     then let splits = split h.Location |> List.map (createOctant bots)
                          let queue = List.append t splits
                                      |> List.sortByDescending (fun o -> o.InRangeCount)
                          findSolution queue solution
                     else findSolution t solution

let queue = [fullOctant]

let bestOctant = findSolution queue None

let result2 = distFromOctant (Option.get bestOctant).Location

