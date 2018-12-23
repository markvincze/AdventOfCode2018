open System.IO

type Acre = 
| Open
| Trees
| Lumber

let parse c = match c with
              | '.' -> Open
              | '|' -> Trees
              | '#' -> Lumber

let map = File.ReadAllLines "FSharp/18-settlers-input.txt"
          |> Array.map (fun l -> l |> Seq.map parse |> Seq.toArray)
          |> (fun rows -> Array2D.init (Array.length rows.[0]) (Array.length rows) (fun x y -> rows.[y].[x]))

let printMap map =
    for y = 0 to (Array2D.length2 map) - 1 do
        for x = 0 to (Array2D.length1 map) - 1 do
            let c = match map.[x, y] with
                    | Open -> '.'
                    | Trees -> '|'
                    | Lumber -> '#'
            printf "%c" c
        printfn ""
    printfn ""


let allElements map =
    [ for x = 0 to (Array2D.length1 map) - 1 do
        for y = 0 to (Array2D.length2 map) - 1 do
            yield map.[x, y] ]

let at map (x, y) =
    if x >= 0 && y >= 0 && x < (Array2D.length1 map) && y < (Array2D.length2 map)
    then Some map.[x, y]
    else None

let neighbors map (x, y) = 
    [(x - 1, y - 1); (x, y - 1); (x + 1, y - 1); (x + 1, y); (x + 1, y + 1); (x, y + 1); (x - 1, y + 1); (x - 1, y);]
    |> List.map (at map)

let grow map =
    let newMap = Array2D.copy map
    for x = 0 to (Array2D.length1 map) - 1 do
        for y = 0 to (Array2D.length2 map) - 1 do
            let neighbors = neighbors map (x, y)
            let newState = match map.[x, y] with
                           | Open -> if neighbors |> List.filter (fun n -> n = Some Trees) |> List.length >= 3
                                     then Trees
                                     else Open
                           | Trees -> if neighbors |> List.filter (fun n -> n = Some Lumber) |> List.length >= 3
                                      then Lumber
                                      else Trees
                           | Lumber -> if neighbors |> List.filter (fun n -> n = Some Lumber) |> List.length >= 1 && neighbors |> List.filter (fun n -> n = Some Trees) |> List.length >= 1
                                       then Lumber
                                       else Open
            newMap.[x, y] <- newState

    newMap

let rec growFor map n =
    match n with
    | 0 -> map
    | n -> growFor (grow map) (n - 1)

// let after10 = growFor map 10

let lumberValue map = (map |> allElements |> List.filter (fun a -> a = Lumber) |> List.length) *
                      (map |> allElements |> List.filter (fun a -> a = Trees) |> List.length)
// let result1 = lumberValue after10

let rec test map n round previousValue =
    let after = growFor map 1000
    let value = lumberValue after
    printfn "Value after %d: %d, Growth: %d" round value (value - previousValue)
    match n with
    | 0 -> ()
    | _ -> test after (n - 1) (round + 1000) value
