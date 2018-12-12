open System
open System.Diagnostics

let serial = 6392

let powerLevel x y =
    let rackId = x + 10
    let value = (rackId * y + serial ) * rackId
    (value / 100) % 10 - 5

let grid = Array2D.init 300 300 (fun x y -> powerLevel (x + 1) (y + 1))

let squarePower cornerX cornerY size =
    [ for x = cornerX to cornerX + size - 1 do
          for y = cornerY to cornerY + size - 1 do
              yield grid.[x - 1, y - 1] ]
    |> List.sum

[<EntryPoint>]
let main argv =
    let sw = Stopwatch.StartNew ()
    let (x, y, s) = [ for size = 1 to 300 do
                        for x = 1 to (300 - size + 1) do
                            for y = 1 to (300 - size + 1) do
                                yield (x, y, size) ]
                    |> List.maxBy (fun (x, y, size) -> squarePower x y size)
    sw.Stop ()
    printfn "Result: %d, %d, %d, Elapsed: %s" x y s (string sw.Elapsed)
    0
