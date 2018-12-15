open System

let input = 306281
let inputArray = input |> string |> Seq.map string |> Seq.map Int32.Parse |> Seq.toArray
let inputLength = inputArray |> Array.length

let recipes = Array.create 40000000 0

recipes.[0] <- 3
recipes.[1] <- 7

let len = 2

let pos1 = 0
let pos2 = 1

type Digits =
| OneDigit of int
| TwoDigits of int * int

let digits num =
    if num <= 9
    then OneDigit num
    else TwoDigits (1, num - 10)

let cook (recipes : int array) pos1 pos2 len =
    let newDigits = digits (recipes.[pos1] + recipes.[pos2])
    let newLen = match newDigits with
                 | OneDigit d -> recipes.[len] <- d
                                 len + 1
                 | TwoDigits (d1, d2) -> recipes.[len] <- d1
                                         recipes.[len + 1] <- d2
                                         len + 2
    ((pos1 + recipes.[pos1] + 1) % newLen), ((pos2 + recipes.[pos2] + 1) % newLen), newLen

// Part 1
let rec cookUntilEnough recipes pos1 pos2 len =
    if len >= input + 10
    then recipes, len
    else let pos1, pos2, len = cook recipes pos1 pos2 len
         cookUntilEnough recipes pos1 pos2 len

let _, finalLen = cookUntilEnough recipes pos1 pos2 len

let result1 = recipes.[input..input + 9] |> Array.map string |> String.concat ""

// Part2
let rec cookUntilEnough2 (recipes : int array) pos1 pos2 len =
    if len >= inputLength && recipes.[len-inputLength..len-1] = inputArray
    then len - inputLength
    else if len >= (inputLength + 1) && recipes.[len-inputLength-1..len-2] = inputArray
    then len - inputLength - 1
    else let pos1, pos2, len = cook recipes pos1 pos2 len
         cookUntilEnough2 recipes pos1 pos2 len

let result2 = cookUntilEnough2 recipes pos1 pos2 len