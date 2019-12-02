open System
open System.IO

type Star = int array

let stars = File.ReadAllLines("FSharp/25-four-dimension-input.txt")
            |> Array.map (fun l -> l.Split(',') |> Array.map Int32.Parse)

printfn "Stars: %A" stars

let dist (s1 : Star) (s2 : Star) =
    (abs (s1.[0] - s2.[0])) +
    (abs (s1.[1] - s2.[1])) +
    (abs (s1.[2] - s2.[2])) +
    (abs (s1.[3] - s2.[3]))

let stars0 = stars |> Array.mapi (fun i s -> (i, s)) |> Array.sortBy (fun (_, s) -> s.[0])
let stars1 = stars |> Array.mapi (fun i s -> (i, s)) |> Array.sortBy (fun (_, s) -> s.[1])
let stars2 = stars |> Array.mapi (fun i s -> (i, s)) |> Array.sortBy (fun (_, s) -> s.[2])
let stars3 = stars |> Array.mapi (fun i s -> (i, s)) |> Array.sortBy (fun (_, s) -> s.[3])

let constellations = stars |> Array.mapi (fun i _ -> i)

let join i1 i2 (constellations : int array) =
    let c1 = constellations.[i1]
    let c2 = constellations.[i2]
    for i in 0..(Array.length constellations) - 1 do
        if constellations.[i] = c2
        then constellations.[i] <- c1
        else ()

let rec expand (stars : (int * Star) array) constellations coord initialIndex index =
    let i1, s1 = stars.[initialIndex]
    let i2, s2 = stars.[index]

    if (abs (s1.[coord] - s2.[coord])) > 3
    then ()
    else ()
         if (dist s1 s2) <= 3
         then (join i1 i2 constellations)

         if index < ((Array.length stars) - 1)
         then expand stars constellations coord initialIndex (index + 1)
         else ()


let scan (stars : (int * Star) array) coord =
    for i in 0..((Array.length stars) - 2) do
        expand stars constellations coord i (i + 1)

scan stars0 0
scan stars1 1
scan stars2 2
scan stars3 3

let result = constellations |> Array.distinct |> Array.length
