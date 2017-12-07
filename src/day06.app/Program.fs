open System

let inputExample =
    "0 2 7 0"

let input =
    "2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14"

let deserialize (input: string) =
    input.Trim().Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(int)

// Domain Types
type Block = Block
type MemoryBank = Block list
type Slot = Index of int
type Memory = Map<Slot,MemoryBank>
type CycleHistory = Memory list
type NumberOfBlocks  = NumberOfBlocks of int
type ChosenMemoryBank = (Slot * NumberOfBlocks) Option

let mkMemory (memory: int []) : Memory =
    memory
    |> Seq.mapi(fun idx length -> Index idx, List.init length (fun _ -> Block))
    |> Map.ofSeq


let chooseMemoryBank (chosen: ChosenMemoryBank) (slot: Slot) (blocks: MemoryBank) : ChosenMemoryBank =
    match chosen with
    // Continue with previously chosen
    | Some (_, NumberOfBlocks n) when n >= blocks.Length -> chosen
    // Continue with the new chosen memory bank
    | _ -> Some (slot, NumberOfBlocks blocks.Length)

let nextSlot slot numberOfSlots =
    match slot with
        Index i -> Index ((i + 1) % (numberOfSlots))

let distributeBlocks (chosen: ChosenMemoryBank) (memory: Memory) : Memory =
    let numberOfSlots = memory.Count
    let rec loop (slot: Slot) (remainingBlocks: MemoryBank) (newMemory: Memory) =
        if remainingBlocks.Length = 0
        then newMemory
        else
            match newMemory.TryFind slot with
            | Some(blocks) ->
                loop
                    <| nextSlot slot numberOfSlots // Get the next slot
                    <| remainingBlocks.Tail // Remove one from the remaing blocks
                    <| Map.add slot (remainingBlocks.Head::blocks) newMemory // Distribute 1 block
            | _ -> memory // Error -> give back the old memory

    match chosen with
    | Some(slot, _) ->
        match memory.TryFind slot with
        | Some(blocks) ->
            loop (nextSlot slot numberOfSlots) blocks (Map.add slot [] memory)
        | _ -> memory // Error -> give back the old memory
    | _ -> memory // Error -> give back the old memory




let redistributeMemory (memory: Memory) =
    let chosen = Map.fold (chooseMemoryBank) None memory
    // printfn "chosen <=== %A" chosen
    let newMemory = distributeBlocks chosen memory
    newMemory

let distributeAlgo (memory: Memory) =
    // printfn "distributeAlgo <=== %A" memory
    let rec loop cycleHistory =
        let lengthCycleHistory = List.length cycleHistory
        let lengthUniqueCycleHistory = Set.count <| Set.ofList cycleHistory
        // printfn "lengthCycleHistory <=== %A" lengthCycleHistory
        // printfn "lengthUniqueCycleHistory <=== %A" lengthUniqueCycleHistory
        if  lengthCycleHistory <> lengthUniqueCycleHistory
        then
            lengthUniqueCycleHistory
            |> printfn "Infinite loop detected after the %ith block redistribution cycle !!"
            let duplicateState = cycleHistory.Head
            printfn "duplicateState <=== %A" (duplicateState |> Map.map (fun k (v: MemoryBank) -> v.Length))

            let fstIndex = cycleHistory |> List.rev |> List.findIndex(fun m -> m = duplicateState)
            let sndIndex = cycleHistory |> List.rev |> List.findIndexBack(fun m -> m = duplicateState)
            printfn "Diff duplicate state between fstIndex (%i) and sndIndex (%i) is %i !!"
                <| fstIndex
                <| sndIndex
                <| sndIndex - fstIndex
            memory
        else
            // printfn "cycleHistory.Head <=== %A" (cycleHistory.Head |> Map.map (fun k (v: MemoryBank) -> v.Length))
            loop ((redistributeMemory cycleHistory.Head) :: cycleHistory)
    loop [memory]

let showMemoryReallocation (memory: int []) =
    memory
    |> mkMemory
    // |> Map.map (fun _ (v: MemoryBank) -> v.Length  )
    |> distributeAlgo

[<EntryPoint>]
let main _ =
    input
    |> deserialize
    |> showMemoryReallocation
    |> Map.toList
    |> List.iter (printfn "%A")
    0 // return an integer exit code

(*
For example, imagine a scenario with only four memory banks:

The banks start with 0, 2, 7, and 0 blocks. The third bank has the most blocks, so it is chosen for redistribution.
Starting with the next bank (the fourth bank) and then continuing to the first bank, the second bank, and so on, the 7 blocks are spread out over the memory banks. The fourth, first, and second banks get two blocks each, and the third bank gets one back. The final result looks like this: 2 4 1 2.
Next, the second bank is chosen because it contains the most blocks (four). Because there are four memory banks, each gets one block. The result is: 3 1 2 3.
Now, there is a tie between the first and fourth memory banks, both of which have three blocks. The first bank wins the tie, and its three blocks are distributed evenly over the other three banks, leaving it with none: 0 2 3 4.
The fourth bank is chosen, and its four blocks are distributed such that each of the four banks receives one: 1 3 4 1.
The third bank is chosen, and the same thing happens: 2 4 1 2.
At this point, we've reached a state we've seen before: 2 4 1 2 was already seen. The infinite loop is detected after the fifth block redistribution cycle, and so the answer in this example is 5.
*)