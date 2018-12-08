open System
open System.IO

type Node = {
    Children : Node list
    Metadata : int list
}

let input = File.ReadAllText("FSharp/08-memory-input.txt").Split([|' '|])
            |> Array.map Int32.Parse
            |> Array.toList

let readHeader input =
    let c :: (m :: rest) = input
    (c, m), rest

let readMetadata input count =
    let rec readMetadataRec input acc count =
        match count with
        | 0 -> acc, input
        | _ -> let h :: t = input
               readMetadataRec t (h :: acc) (count - 1)
    readMetadataRec input [] count

let rec readNode input =
    let (c, m), input = readHeader input
    let (children, input) = [1..c]
                            |> List.fold
                                 (fun (children, input) _ -> let node, input = readNode input
                                                             (node :: children, input))
                                 ([], input)
    let (metadata, input) = readMetadata input m
    
    let node = {
        Children = children |> List.rev
        Metadata = metadata
    }

    node, input
           
let tree, _ = readNode input

let rec sumMetadata node =
    let childSum = node.Children |> List.sumBy sumMetadata
    childSum + (node.Metadata |> List.sum)

let result1 = sumMetadata tree

let rec nodeValue node =
    match node.Children |> List.length with
    | 0 -> node.Metadata |> List.sum
    | _ -> node.Metadata
           |> List.sumBy (fun m -> match m with
                                   | m when m > 0 && m <= (List.length node.Children) -> node.Children |> List.item (m - 1) |> nodeValue
                                   | _ -> 0)

let result2 = tree |> nodeValue