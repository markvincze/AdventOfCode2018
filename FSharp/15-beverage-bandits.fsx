open System.IO

type Unit = {
    Health : int
    MovedInRound : int
}

type Tile =
| Empty
| Wall
| Goblin of Unit
| Elf of Unit

let parseTile c = match c with
                  | '.' -> Empty
                  | '#' -> Wall
                  | 'E' -> Elf { Health = 200; MovedInRound = -1 }
                  | 'G' -> Goblin { Health = 200; MovedInRound = -1 }
                  | _ -> failwith "Invalid input"

let mapRotated = File.ReadAllLines("FSharp/15-beverage-bandits-input.txt")
                 |> Array.map (fun l -> l |> Seq.map parseTile)
                 |> array2D

let mapWidth = Array2D.length2 mapRotated
let mapHeight = Array2D.length1 mapRotated

let map = Array2D.init mapWidth mapHeight (fun x y -> mapRotated.[y, x])

let printMap (map : Tile[,]) =
    for y in 0..(mapHeight - 1) do
        for x in 0..(mapWidth - 1) do
            let c = match map.[x, y] with
                    | Empty -> '.'
                    | Wall -> '#'
                    | Elf _ -> 'E'
                    | Goblin _ -> 'G'
            printf "%c" c
        printfn ""
    printfn ""

let isEmpty tile = match tile with
                   | Empty -> true
                   | _ -> false

let isElf tile = match tile with
                 | Elf _ -> true
                 | _ -> false

let isGoblin tile = match tile with
                    | Goblin _ -> true
                    | _ -> false

let getHealth tile = match tile with
                     | Elf unit | Goblin unit -> unit.Health
                     | _ -> failwith "Invalid input for getHealth"

let neighbors (x, y) = [ (x, y - 1); (x - 1, y); (x + 1, y); (x, y + 1) ]

let getAdjTarget (map : Tile[,]) (x, y) selector =
    (x, y) |> neighbors
    |> List.filter (fun (x, y) -> map.[x, y] |> selector)
    |> List.sortBy (fun (x, y) -> map.[x, y] |> getHealth)
    |> List.tryHead

let findTargets (map : Tile[,]) selector =
    [ for x in 0..mapWidth - 1 do
        for y in 0..mapHeight - 1 do
            yield (x, y)]
    |> Seq.filter (fun (x, y) -> map.[x, y] |> selector)
    |> Seq.collect (fun (x, y) -> [ (x - 1, y); (x, y - 1); (x + 1, y); (x, y + 1) ])
    |> Seq.filter (fun (x, y) -> map.[x, y] |> isEmpty)
    |> Seq.distinct

let isEnemyLeft (map : Tile[,]) enemySelector =
    [ for x in 0..mapWidth - 1 do
        for y in 0..mapHeight - 1 do
            yield (x, y)]
    |> Seq.exists (fun (x, y) -> map.[x, y] |> enemySelector) 

type MoveStepResult =
| FoundTarget of ((int * int) * (int * int))
| NewSteps of ((int * int) * ((int * int) option)) list

let move map (x, y) enemySelector =
    let queue = [ ((x, y), None) ]
    let visited = Array2D.create mapWidth mapHeight false

    let rec findMove (map : Tile[,]) queue (visited : bool[,]) enemySelector =
        let moveStep ((x, y), firstStep) =
            visited.[x, y] <- true
            let foundEnemy =
                (x, y) |> neighbors
                |> List.filter (fun (x, y) -> map.[x, y] |> enemySelector)
                |> List.tryHead
            
            match foundEnemy with
            | Some _ -> FoundTarget ((x, y), Option.get firstStep)
            | None -> let newSteps = (x, y) |> neighbors
                                     |> List.filter (fun (x, y) -> map.[x, y] |> isEmpty)
                                     |> List.filter (fun (x, y) -> not visited.[x, y])
                                     |> List.map (fun pos -> match firstStep with
                                                             | None -> pos, Some pos
                                                             | Some _ -> pos, firstStep)
                      NewSteps newSteps

        let queueResults = queue |> List.map moveStep

        let foundTargets = queueResults |> List.choose (fun r -> match r with | FoundTarget t -> Some t | _ -> None)

        if List.length queueResults = 0
        then []
        else if List.length foundTargets > 0
        then foundTargets
        else let newQueue = queueResults
                            |> List.choose (fun r -> match r with | NewSteps ts -> Some ts | _ -> None)
                            |> List.collect id
                            |> List.distinct
             findMove map newQueue visited enemySelector

    let options = findMove map queue visited enemySelector

    options
    |> List.sortBy (fun ((xt, yt), (xf, yf)) -> (yt, xt, yf, xf))
    |> List.map snd
    |> List.tryHead

type AttackResult =
| CouldNotAttack
| Attacked
| ElfDied

let attack attackPower target =
    match target with
    | Goblin unit | Elf unit -> let newHealth = unit.Health - attackPower
                                if newHealth > 0
                                then match target with
                                     | Elf unit -> Elf { unit with Health = newHealth }
                                     | Goblin unit -> Goblin { unit with Health = newHealth }
                                else Empty
    | _ -> failwith "Can't attack this tile"

let tryAttack (map : Tile[,]) (x, y) enemySelector attackPower =
    let adjTarget = getAdjTarget map (x, y) enemySelector
    match adjTarget with
    | Some (tx, ty) -> let target = map.[tx, ty]
                       map.[tx, ty] <- attack attackPower target
                       match target, map.[tx, ty] with
                       | Elf _, Empty -> ElfDied
                       | _ -> Attacked
    | None -> CouldNotAttack

let progress (map : Tile[,]) round elfAttackPower =
    let mutable noEnemyLeft = false
    let mutable elfDied = false

    for y in 0..mapHeight - 1 do
        for x in 0..mapWidth - 1 do
            let tile = map.[x, y]
            match tile with
            | Elf unit | Goblin unit when unit.MovedInRound <> round ->
                let enemySelector = match tile with
                                    | Elf _ -> isGoblin
                                    | Goblin _ -> isElf
                
                if not <| isEnemyLeft map enemySelector
                then noEnemyLeft <- true
                else ()

                let attackPower = match tile with | Elf _ -> elfAttackPower | Goblin _ -> 3

                match tryAttack map (x, y) enemySelector attackPower with
                | ElfDied -> elfDied <- true
                | Attacked -> match tile with
                              | Elf unit -> map.[x, y] <- Elf { unit with MovedInRound = round}
                              | Goblin unit -> map.[x, y] <- Goblin { unit with MovedInRound = round}
                | CouldNotAttack -> let newPos = move map (x, y) enemySelector
                                    match newPos with
                                    | Some (xn, yn) -> map.[x, y] <- Empty
                                                       match tile with
                                                       | Elf unit -> map.[xn, yn] <- Elf { unit with MovedInRound = round }
                                                       | Goblin unit ->  map.[xn, yn] <- Goblin { unit with MovedInRound = round }
                                                       tryAttack map (xn, yn) enemySelector attackPower |> ignore
                                    | None -> ()
            | _ -> ()

    map, noEnemyLeft, elfDied
    // printMap map, actionHappened

let rec progressUntilEnd map round attackPower =
    let map, noEnemyLeft, elfDied = progress map round attackPower

    match noEnemyLeft, elfDied with
    | false, false -> progressUntilEnd map (round + 1) attackPower
    | _, true -> false, round - 1
    | true, false -> true, round - 1

let findMinimumEnoughAttackPower map =
    let originalMap = Array2D.copy map
    let rec findMinimumEnoughAttackPowerRec map attackPower =
        match progressUntilEnd map 1 attackPower with
        | false, _ -> 
            let map = Array2D.copy originalMap
            findMinimumEnoughAttackPowerRec map (attackPower + 1)
        | true, gameLength -> gameLength, attackPower, map
    
    findMinimumEnoughAttackPowerRec map 4

let allHealth (map : Tile[,]) =
    [ for x in 0..mapWidth-1 do
        for y in 0..mapHeight-1 do
            yield (x, y)]
    |> Seq.sumBy (fun (x, y) -> match map.[x, y] with | Elf unit | Goblin unit -> unit.Health | _ -> 0)

// let gameLength = progressUntilEnd map 1
let gameLength, attackPower, endMap = findMinimumEnoughAttackPower map
let totalHealth = allHealth endMap
let result2 = gameLength * totalHealth