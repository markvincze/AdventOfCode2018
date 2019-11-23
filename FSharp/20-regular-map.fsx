open System.IO

type Queue<'a> =
    | Queue of 'a list * 'a list

let emptyQueue = Queue([], [])

let enqueue q e = 
    match q with
    | Queue(fs, bs) -> Queue(e :: fs, bs)

let dequeue q = 
    match q with
    | Queue([], []) -> failwith "Empty queue!"
    | Queue(fs, b :: bs) -> b, Queue(fs, bs)
    | Queue(fs, []) -> 
        let bs = List.rev fs
        bs.Head, Queue([], bs.Tail)

type Stack<'T> = StackContents of 'T list

let push x aStack =
    let (StackContents contents) = aStack
    let newContents = x::contents
    StackContents newContents

let pop (StackContents contents) =
    match contents with
    | top::rest ->
        let newStack = StackContents rest
        (top,newStack)
    | [] -> 
        failwith "Stack underflow"

let peek (StackContents contents) =
    match contents with
    | top::_ -> top
    | [] -> failwith "Stack underflow"

let input = File.ReadAllText("FSharp/20-regular-map-input.txt")

type Connection =
| Door
| Wall

type Room = {
    East : Connection
    North : Connection
    West : Connection
    South : Connection
}

let defaultRoom = { East = Wall; North = Wall; West = Wall; South = Wall }

type Coordinate = {
    X : int
    Y : int
}

type Direction =
| East
| North
| West
| South

let westFrom coord = { X = coord.X - 1; Y = coord.Y }
let northFrom coord = { X = coord.X; Y = coord.Y - 1 }
let eastFrom coord = { X = coord.X + 1; Y = coord.Y }
let southFrom coord = { X = coord.X; Y = coord.Y + 1 }

let roomAt rooms coord =
    match Map.tryFind coord rooms with
    | Some r -> r, rooms

    | None -> let west = match Map.tryFind (westFrom coord) rooms with
                         | Some { East = Door } -> Door
                         | _ -> Wall
              let north = match Map.tryFind (northFrom coord) rooms with
                          | Some { South = Door } -> Door
                          | _ -> Wall
              let east = match Map.tryFind (eastFrom coord) rooms with
                         | Some { West = Door } -> Door
                         | _ -> Wall
              let south = match Map.tryFind (southFrom coord) rooms with
                          | Some { North = Door } -> Door
                          | _ -> Wall

              let newRoom = { West = west; North = north; East = east; South = south }

              newRoom, (Map.add coord newRoom rooms)

let walled room = room.East = Wall && room.North = Wall && room.West = Wall && room.South = Wall

let print (rooms : Map<Coordinate, Room>) =
    let minX = rooms |> Map.toSeq |> Seq.map (fun (coord, _) -> coord.X) |> Seq.min
    let minY = rooms |> Map.toSeq |> Seq.map (fun (coord, _) -> coord.Y) |> Seq.min
    let maxX = rooms |> Map.toSeq |> Seq.map (fun (coord, _) -> coord.X) |> Seq.max
    let maxY = rooms |> Map.toSeq |> Seq.map (fun (coord, _) -> coord.Y) |> Seq.max

    printfn "X: %d to %d, Y: %d to %d" minX maxX minY maxY

    for x in minX..maxX do
        printf "##"
    printfn ""

    for y in minY..maxY do
        printf "#"
        for x in minX..maxX do
            let room, rooms = roomAt rooms { X = x; Y = y }

            if x = 0 && y = 0
            then printf "X"
            else if walled room
                 then printf "#"
                 else printf "."

            if room.East = Wall
            then printf "#"
            else printf "|"

        printfn ""
        printf "#"
        for x in minX..maxX do
            let room, rooms = roomAt rooms { X = x; Y = y }
            if room.South = Wall
            then printf "#"
            else printf "-"

            printf "#"

        printfn ""

let connection room direction = match direction with
                                | East -> room.East
                                | North -> room.North
                                | West -> room.West
                                | South -> room.South

let move coord direction = match direction with
                           | West -> westFrom coord
                           | North -> northFrom coord
                           | East -> eastFrom coord
                           | South -> southFrom coord

let opposite direction = match direction with
                         | West -> East
                         | North -> South
                         | East -> West
                         | South -> North

let rooms = Map.empty<Coordinate, Room> |> Map.add { X = 0; Y = 0 } defaultRoom

let step position rooms direction =
    let newPosition = move position direction
    let room, rooms = roomAt rooms position

    if connection room direction = Door
    then newPosition, rooms
    else let newRoom, rooms = roomAt rooms newPosition
         match direction with
         | West -> let rooms = Map.add position { room with West = Door } rooms
                   let rooms = Map.add newPosition { newRoom with East = Door } rooms
                   newPosition, rooms
         | North -> let rooms = Map.add position { room with North = Door } rooms
                    let rooms = Map.add newPosition { newRoom with South = Door } rooms
                    newPosition, rooms
         | East -> let rooms = Map.add position { room with East = Door } rooms
                   let rooms = Map.add newPosition { newRoom with West = Door } rooms
                   newPosition, rooms
         | South -> let rooms = Map.add position { room with South = Door } rooms
                    let rooms = Map.add newPosition { newRoom with North = Door } rooms
                    newPosition, rooms

let parseDirection c = match c with
                       | 'E' -> East
                       | 'N' -> North
                       | 'W' -> West
                       | 'S' -> South
                       | _ -> failwith "Invalid input"

let rec processSegment position rooms input index =
    if index < String.length input
    then let direction = input.[index] |> parseDirection
         let position, rooms = step position rooms direction
         processSegment position rooms input (index + 1)
    else position, rooms

let findClosingParen input index =
    let rec findClosingParenRec (input : string) index level =
        match input.[index] with
        | '(' -> findClosingParenRec input (index + 1) (level + 1)
        | ')' -> if level = 0
                 then index
                 else findClosingParenRec input (index + 1) (level - 1)
        | _ -> findClosingParenRec input (index + 1) level
    
    findClosingParenRec input (index + 1) 0

let rec processS position groupStartPositions rooms input index =
    if index < String.length input
    then match input.[index] with
         | '(' -> processS position (push position groupStartPositions) rooms input (index + 1)
         | '|' -> let rooms = processS (peek groupStartPositions) groupStartPositions rooms input (index + 1)
                  let closingParenIndex = findClosingParen input index
                  processS position ((pop groupStartPositions) |> snd) rooms input (closingParenIndex + 1)
         | ')' -> rooms
         | c -> let direction = c |> parseDirection
                let position, rooms = step position rooms direction
                processS position groupStartPositions rooms input (index + 1)
    else rooms

let rec furthestRoom rooms queue visited maxDist over1000 =
    printfn "Calling furthestRoom"
    if queue = emptyQueue
    then maxDist, over1000
    else let ((c, dist), queue) = dequeue queue
         let room, rooms = roomAt rooms c
         printfn "Current room: %A" room
         let queue, visited =
             if room.West = Door && (not (Set.contains (westFrom c) visited))
             then (enqueue queue ((westFrom c), (dist + 1))), (Set.add (westFrom c) visited)
             else queue, visited
         let queue, visited =
             if room.North = Door && (not (Set.contains (northFrom c) visited))
             then enqueue queue ((northFrom c), (dist + 1)), Set.add (northFrom c) visited
             else queue, visited
         let queue, visited =
             if room.East = Door && (not (Set.contains (eastFrom c) visited))
             then enqueue queue ((eastFrom c), (dist + 1)), Set.add (eastFrom c) visited
             else queue, visited
         let queue, visited =
             if room.South = Door && (not (Set.contains (southFrom c) visited))
             then enqueue queue ((southFrom c), (dist + 1)), Set.add (southFrom c) visited
             else queue, visited

         furthestRoom rooms queue visited (max dist maxDist) (if dist >= 1000 then over1000 + 1 else over1000)


let initialGroupStartPositions = StackContents [ { X = 0; Y = 0 } ]
// let testInput = "ENWWWNEEE"
// let testInput = "ENWWW(NEEE|SSE(EE|N))"
let testInput = "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))"
let endRooms = processS { X = 0; Y = 0 } initialGroupStartPositions Map.empty<Coordinate,Room> input 0
let furthest, over1000 = furthestRoom endRooms (enqueue emptyQueue ({ X = 0; Y = 0 }, 0)) Set.empty 0 0

// let pos, rooms = processSegment { X = 0; Y = 0 } Map.empty<Coordinate,Room> testInput 0 |> snd |> print;;
//let rooms = processS { X = 0; Y = 0 } initialGroupStartPositions Map.empty<Coordinate,Room> testInput 0 |> print;;
// processS { X = 0; Y = 0 } initialGroupStartPositions Map.empty<Coordinate,Room> testInput 0 |> print;;