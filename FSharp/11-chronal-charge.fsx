let serial = 6392

let powerLevel x y =
    let rackId = x + 10
    let value = (rackId * y + serial ) * rackId
    (value / 100) % 10 - 5

let grid = Array2D.init 300 300 (fun x y -> powerLevel (x + 1) (y + 1))

let squarePower cornerX cornerY size =
    [ for x = cornerX to cornerX + size - 1 do
          for y = cornerY to cornerY + size - 1 do
            //   yield grid.[x - 1, y - 1] ]
              yield powerLevel x y ]
    |> List.sum

// let values = [ for x = 1 to 298 do
//                    for y = 1 to 298 do
//                        yield (x, y) ]
//              |> List.map squarePower

let result1 = [ for size = 1 to 50 do
                  printfn "Testing for size %d" size
                  for x = 1 to (300 - size + 1) do
                      for y = 1 to (300 - size + 1) do
                          yield (x, y, size) ]
              |> List.maxBy (fun (x, y, size) -> squarePower x y size)