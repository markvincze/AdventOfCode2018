type Marble = {
    Value : int64
    mutable Left : Marble
    mutable Right : Marble
}

let playerCount = 419
let lastMarble = 72164L

let playerScores = Map.ofSeq [for i in 0..playerCount -> (i, 0L)]

let rec circle = {
    Value = 0L
    Left = circle
    Right = circle
}

let step circle nextValue playerScores playerIndex =
    match nextValue % 23L with
    | 0L -> let toRemove = circle.Left.Left.Left.Left.Left.Left.Left
            toRemove.Left.Right <- toRemove.Right
            toRemove.Right.Left <- toRemove.Left

            toRemove.Right, (Map.add playerIndex ((Map.find playerIndex playerScores) + nextValue + toRemove.Value) playerScores)
    | _ -> let left = circle.Right
           let right = circle.Right.Right
           let newMarble = {
               Value = nextValue
               Right = right
               Left = left
           }
           left.Right <- newMarble
           right.Left <- newMarble

           newMarble, playerScores

let rec play circle lastMarble nextValue playerScores playerIndex =
    let circle, playerScores = step circle nextValue playerScores playerIndex
    if nextValue = lastMarble
    then playerScores |> Seq.sortByDescending (fun kvp -> kvp.Value ) |> Seq.head
    else play circle lastMarble (nextValue + 1L) playerScores ((playerIndex + 1) % playerCount)

let result1 = play circle lastMarble 1L playerScores 0
let result2 = play circle (lastMarble * 100L) 1L playerScores 0