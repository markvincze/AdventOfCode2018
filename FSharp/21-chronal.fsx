open System
open System.IO
open System.Text.RegularExpressions

type Op =
| Addr
| Addi
| Mulr
| Muli
| Banr
| Bani
| Borr
| Bori
| Setr
| Seti
| Gtir
| Gtri
| Gtrr
| Eqir
| Eqri
| Eqrr

type Instruction = Op * (int64 array)

let parse (line : string) =
    let op = line.Substring(0, 4)
    let args = line.Substring(4).Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map Int64.Parse
    match op with
    | "addr" -> Addr, args
    | "addi" -> Addi, args
    | "mulr" -> Mulr, args
    | "muli" -> Muli, args
    | "banr" -> Banr, args
    | "bani" -> Bani, args
    | "borr" -> Borr, args
    | "bori" -> Bori, args
    | "setr" -> Setr, args
    | "seti" -> Seti, args
    | "gtir" -> Gtir, args
    | "gtri" -> Gtri, args
    | "gtrr" -> Gtrr, args
    | "eqir" -> Eqir, args
    | "eqri" -> Eqri, args
    | "eqrr" -> Eqrr, args
    | _ -> failwithf "Invalid opcode: %s" op

let lines = File.ReadAllLines "FSharp/21-chronal-input.txt"

let boundReg = lines.[0] |> Seq.last |> string |> Int32.Parse

let program = lines |> Array.skip 1 |> Array.map parse

type State = int * (int64 array)

type Result =
| Executed of State
| Stopped

let validRegIndex i = i >= 0L && i <= 5L

let execute (program : Instruction array) (ip, (regs : int64 array))  =
    if ip < 0 || ip >= (Array.length program)
    then Stopped
    else let (op, args) = program.[ip]
         //  if ip > 18
         //  then printfn "IP > 18"
         //  else ()
        //  printfn "Executing ip %d: %A" ip (op, args)

        //  if op = Eqrr
        //  then printfn "Executing eqrr, %A" (ip, regs)
        //  else ()
         if ip = 28 && regs.[3] = 13270004L
         then printfn "Executing ip %d: %A, Regs: %A" ip (op, args) regs
         else ()

         //  if ip = 11
         //  then printfn "Small jump. State: %d, %A" ip regs
         //  else if ip = 15
         //  then printfn "Large jump. State: %d, %A" ip regs
         //  else ()
         //   printfn "Op: %A, %A. State: %d, %A" op args ip regs
         let newRegs = regs |> Array.copy
         newRegs.[boundReg] <- (int64 ip)

         let val1 = args.[0]
         let val2 = args.[1]
         let regVal1 = if args.[0] |> validRegIndex then newRegs.[int args.[0]] else 0L
         let regVal2 = if args.[1] |> validRegIndex then newRegs.[int args.[1]] else 0L
         let output = match op with
                      | Addr -> regVal1 + regVal2
                      | Addi -> regVal1 + val2
                      | Mulr -> regVal1 * regVal2
                      | Muli -> regVal1 * val2
                      | Banr -> (regVal1 &&& regVal2)
                      | Bani -> (regVal1 &&& val2)
                      | Borr -> (regVal1 ||| regVal2)
                      | Bori -> (regVal1 ||| val2)
                      | Setr -> regVal1
                      | Seti -> val1
                      | Gtir -> if val1 > regVal2 then 1L else 0L
                      | Gtri -> if regVal1 > val2 then 1L else 0L
                      | Gtrr -> if regVal1 > regVal2 then 1L else 0L
                      | Eqir -> if val1 = regVal2 then 1L else 0L
                      | Eqri -> if regVal1 = val2 then 1L else 0L
                      | Eqrr -> if regVal1 = regVal2 then 1L else 0L

         newRegs.[int args.[2]] <- output

        //  if args.[2] = 0L
        //  then printfn "Changing reg 0. Op: %A, %A. State: %d, %A" op args ip regs
        //  else ()

         Executed (newRegs.[boundReg] + 1L |> int, newRegs)

let rec run program (state : State) (r3s : Set<int64>) =
    let result = execute program state

    let r3s = let (ip, regs) = state
              if ip = 28
              then printfn "R3s length: %d, new r3: %d" (Set.count r3s) regs.[3]
                   Set.add regs.[3] r3s
              else r3s

    match result with
    | Stopped -> state
    | Executed newState -> run program newState r3s

// let regs2 = [|13270004L; 0L; 0L; 0L; 0L; 0L|]
let regs2 = [|0L; 0L; 0L; 0L; 0L; 0L|]
// let result2 = run program (0, regs2)

