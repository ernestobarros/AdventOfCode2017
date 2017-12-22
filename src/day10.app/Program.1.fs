let circularExample = [0; 1; 2; 3; 4]
// let inputExample = [3; 4; 1; 5]
let inputExample = [3;]

type State = {StartPos: int; SkipAcc: int; CircularList: int list;}

let prevPos (pos: int) (len: int) =
    pos - 1
    |> fun prevPos -> if prevPos > 0 then prevPos else len

let nextPos (pos: int) (len: int) =
    pos + 1
    |> fun nextPos -> if nextPos < len then nextPos else (nextPos - len) % nextPos

let mkCircularList (startPos: int) (endPos: int) (circularList: int list) =
    let len = circularList.Length
    let rec loop (posA: int) (posB: int) (xs: int list) =
        match posA.CompareTo(posB) with
            | 0 -> xs
            | -1 ->
                List.concat
                    [
                        xs.[0..posA-1]
                        [xs.[posB]]
                        xs.[posA+1..posB-1]
                        [xs.[posA]]
                        xs.[posB+1..len]
                    ]
                |> fun xs1 ->
                    if (posA+1).CompareTo(posB-1) = 1
                    then xs1
                    else loop (posA+1) (posB-1) xs1
            | 1 -> xs
            | _ -> failwith "Mission impossible !!"
    loop startPos endPos circularList

let folder (state: State) (size: int) =

    let len = state.CircularList.Length
    let nextStartPos =
        (state.StartPos + state.SkipAcc + size)
        |> fun pos -> if pos < len then pos else pos - len % pos

    (* Nice try .. but what if starPos and endPos overlap each other *)
    (* Luckily within the current input set this does not happen !! *)
    // let (l1, l2) =
    //     let endPos = (nextStartPos - 1) - state.SkipAcc
    //     state.CircularList
    //     |> List.mapi(fun idx x ->
    //         if state.StartPos <= endPos
    //         then
    //             if idx >= state.StartPos && idx <= endPos
    //             then true, x
    //             else false, x
    //         else
    //             if idx >= state.StartPos || idx <= endPos
    //             then true, x
    //             else false, x
    //     )
    //     |> List.partition(fst)

    // if state.SkipAcc <= 1
    // then
    //     printfn ""
    //     printfn "%A" state.CircularList
    //     printfn "%A" l1
    //     printfn "%A" l2

    {state with
        StartPos = nextStartPos
        SkipAcc = state.SkipAcc + 1}

[<EntryPoint>]
let mainBACKUP _ =
    let state = {StartPos = 0; SkipAcc = 0; CircularList = circularExample;}
    inputExample
    |> List.fold (folder) state
    |> ignore

    0 // return an integer exit code
