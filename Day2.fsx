

let opAdd = 1
let opMultiple = 2
let ophalt = 99

type pointer = int

let opCode1 a b = a + b 
let opCode2 a b = a * b 

let runNextCommand (memory:int[]) (instructionPointer:pointer) : pointer =
  let op = memory.[instructionPointer] 
  let op1 = memory.[memory.[instructionPointer + 1]]
  let op2 = memory.[memory.[instructionPointer + 2]]
  let target = memory.[instructionPointer + 3]
  match op with
    | 1 -> memory.[target] <- opCode1 op1 op2
           instructionPointer + 4
    | 2 -> memory.[target] <- opCode2 op1 op2
           instructionPointer + 4
    | _ -> failwith "Boom!"

let rec run (memory:int[]) (instructionPointer:pointer) = 
  if memory.[instructionPointer] = 99 then
    memory.[0]
  else
    let newInstructionPointer = runNextCommand memory instructionPointer
    run memory newInstructionPointer

  
let memory = [| 1; 0; 0; 0; 99 |]
let memory2 = [| 1; 1; 1; 4; 99; 5; 6; 0; 99 |]


let lastPointer = run memory2 0

lastPointer   

memory2

let dothing verb noun =
    let splitOnComma (str:string) = str.Split ','

    let puzzleMemory = System.IO.File.ReadAllText("day2-puzzle-input.txt")
                        |> splitOnComma
                        |> Array.map int

    puzzleMemory.[1] <- verb
    puzzleMemory.[2] <- noun

    let valueInAddressZero = run puzzleMemory 0
    valueInAddressZero   


dothing 12 2

let items1 = seq { 0 .. 99 }
let items2 = seq { 0 .. 99 }
let flat = items1 |> Seq.collect (fun i1 -> items2 |> Seq.collect (fun i2 -> [i1, i2]))
flat


let (noun, verb) = flat |> Seq.skipWhile (fun (n, v) -> (dothing n v) <> 19690720)
                        |> Seq.head

let answer = 100 * noun + verb


