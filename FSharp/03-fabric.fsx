open System
open System.IO
open System.Text.RegularExpressions

type Claim = {
    Id : int
    X : int
    Y : int
    Width : int
    Height : int
}

// Example: #5 @ 65,785: 13x15
let claimRegex = new Regex("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)", RegexOptions.Compiled)

let parseClaim line = let rm = claimRegex.Match line
                      let group (i : int) = (rm.Groups.[i].Value) |> Int32.Parse

                      {
                          Id = group 1
                          X = group 2
                          Y = group 3
                          Width = group 4
                          Height = group 5
                      }

let claims = File.ReadAllLines("FSharp/03-fabric-input.txt")
             |> Array.map parseClaim

type SquareOccupancy =
| None
| One
| More

// Part 1
let fabric = Array2D.create 1000 1000 None

let increase occupancy = match occupancy with
                         | None -> One
                         | One -> More
                         | More -> More

let markClaim (fabric : SquareOccupancy[,]) claim =
    for x = claim.X to (claim.X + claim.Width - 1) do
        for y = claim.Y to (claim.Y + claim.Height - 1) do
            fabric.[x, y] <- increase fabric.[x, y]

claims |> Array.iter (markClaim fabric)

let result1 = fabric |> Seq.cast |> Seq.filter (fun o -> o = More) |> Seq.length

// Part2
let isClaimFree (fabric : SquareOccupancy[,]) claim =
    [ for x = claim.X to (claim.X + claim.Width - 1) do
            for y = claim.Y to (claim.Y + claim.Height - 1) do
                yield fabric.[x, y] ]
    |> Seq.contains More
    |> not

let result2 = claims |> Array.find (isClaimFree fabric)
