open System
open System.IO

type Track =
| Horizontal
| Vertical
| TopLeftCorner
| TopRightCorner
| BottomRightCorner
| BottomLeftCorner
| Intersection

type Direction =
| Up
| Right
| Down
| Left

type Turn =
| LeftTurn
| Straight
| RightTurn

type Cart = {
    Direction : Direction
    LastTurn : Turn
    MovedInIter : int option
}

let newCart dir = {
    Direction = dir
    LastTurn = RightTurn
    MovedInIter = None
}

type Tile =
| Empty
| Track of Track
| Cart of Cart * Track

let chars = File.ReadAllLines("FSharp/13-mine-cart-input.txt")
            |> Array.map Seq.toArray
            |> array2D

let mapWidth = Array2D.length2 chars
let mapHeight = Array2D.length1 chars

let parseTile (chars : char[,]) (x, y) =
    match chars.[y, x] with
    | ' ' -> Empty
    | '-' -> Track Horizontal
    | '|' -> Track Vertical
    | '^' -> Cart (newCart Up, Vertical)
    | '>' -> Cart (newCart Right, Horizontal)
    | 'v' -> Cart (newCart Down, Vertical)
    | '<' -> Cart (newCart Left, Horizontal)
    | '+' -> Track Intersection
    | '/' -> if x > 0 && (chars.[y, x - 1] = '-'  || chars.[y, x - 1] = '+')
             then Track BottomRightCorner
             else Track TopLeftCorner
    | '\\' -> if x > 0 && (chars.[y, x - 1] = '-'  || chars.[y, x - 1] = '+')
              then Track TopRightCorner
              else Track BottomLeftCorner
    | _ -> failwith "Unknown tile character"

let printMap (map : Tile[,]) =
    for y in 0..(mapHeight - 1) do
        for x in 0..(mapWidth - 1) do
            let tile = match map.[x, y] with
                        | Empty -> ' '
                        | Track t -> match t with
                                     | Horizontal -> '-'
                                     | Vertical -> '|'
                                     | Intersection -> '+'
                                     | TopLeftCorner | BottomRightCorner -> '/'
                                     | BottomLeftCorner | TopRightCorner -> '\\'
                        | Cart (c, _) -> match c.Direction with
                                         | Up -> '^'
                                         | Right -> '>'
                                         | Down -> 'v'
                                         | Left -> '<'
            printf "%c" tile
        printfn ""
    printfn ""
    printfn ""

let map = Array2D.init mapWidth mapHeight (fun x y -> parseTile chars (x, y))

let move dir (x, y) = match dir with
                      | Up -> (x, y - 1)
                      | Right -> (x + 1, y)
                      | Down -> (x, y + 1)
                      | Left -> (x - 1, y)

let turn dir turn =
    match turn with
    | Straight -> dir
    | LeftTurn -> match dir with
                  | Up -> Left
                  | Right -> Up
                  | Down -> Right
                  | Left -> Down
    | RightTurn -> match dir with
                   | Up -> Right
                   | Right -> Down
                   | Down -> Left
                   | Left -> Up

let nextTurn lastTurn = match lastTurn with
                        | LeftTurn -> Straight
                        | Straight -> RightTurn
                        | RightTurn -> LeftTurn

let arrive cart at moveIter =
    match at with
    | Vertical | Horizontal -> { cart with MovedInIter = Some moveIter }
    | Intersection -> let nextTurn = nextTurn cart.LastTurn
                      {
                          Direction = turn cart.Direction nextTurn
                          LastTurn = nextTurn
                          MovedInIter = Some moveIter
                      }
    | TopLeftCorner | BottomRightCorner -> let newDir = match cart.Direction with
                                                        | Up | Down -> turn cart.Direction RightTurn
                                                        | _ -> turn cart.Direction LeftTurn
                                           {
                                               Direction = newDir
                                               LastTurn = cart.LastTurn
                                               MovedInIter = Some moveIter
                                           }
    | TopRightCorner | BottomLeftCorner -> let newDir = match cart.Direction with
                                                        | Up | Down -> turn cart.Direction LeftTurn
                                                        | _ -> turn cart.Direction RightTurn
                                           {
                                               Direction = newDir
                                               LastTurn = cart.LastTurn
                                               MovedInIter = Some moveIter
                                           }

let rec progressUntilCrash (map : Tile[,]) iter =
    let mutable firstCrash : (int * int) option = None

    for y in 0..(mapHeight - 1) do
        for x in 0..(mapWidth - 1) do
            match map.[x, y] with
            | Cart (c, t) -> if c.MovedInIter = Some iter
                             then ()
                             else let xn, yn = move c.Direction (x, y)
                                  map.[x,y] <- Track t
                                  match map.[xn, yn] with
                                  | Cart (cn, tn) -> match firstCrash with
                                                     | None -> firstCrash <- Some (xn, yn)
                                                     | Some _ -> ()

                                                     map.[xn, yn] <- Track tn
                                  | Empty -> failwith "Bug: the cart moved to an empty tile"
                                  | Track tn -> let newCart = arrive c tn iter
                                                map.[xn,yn] <- Cart (newCart, tn)
            | _ -> ()

    let carts =
        [ for y in 0..(mapHeight - 1) do
            for x in 0..(mapWidth - 1) do
                yield (x, y) ]
        |> Seq.map (fun (x, y) -> map.[x, y], x, y)
        |> Seq.filter (fun (t, _, _) -> match t with
                                        | Cart _ -> true
                                        | _ -> false)

    let cartCount = Seq.length carts 
    match cartCount with
    | 1 -> carts |> Seq.head
    | _ -> progressUntilCrash map (iter + 1)

    // match firstCrash with
    // | None -> progressUntilCrash map (iter + 1)
    // | Some crash -> crash

// let firstCrashPos = progressUntilCrash map 0
let lastStanding = progressUntilCrash map 0