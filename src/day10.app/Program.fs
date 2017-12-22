let circularExample = [0; 1; 2; 3; 4]
let inputExample = [3; 4; 1; 5]

let circular = [0 .. 255]
let input = [199; 0; 255; 136; 174; 254; 227; 16; 51; 85; 1; 2; 22; 17; 7; 192]

let logAndContinue = fun x -> printfn "%A" x; x

let realPos (len: int) (pos: int) =
    if pos >= 0
    then
        if pos < len then pos
        else (pos - len) % len
    else
        if abs pos <= len then len + pos
        else len - ((abs pos) % len)
// let realPosL5 = realPos 5
// realPosL5 -1
// |> printfn "expected 4, got %i"
// realPosL5 -1
// |> printfn "expected 4, got %i"
// realPosL5 -2
// |> printfn "expected 3, got %i"
// realPosL5 -5
// |> printfn "expected 0, got %i"
// realPosL5 -6
// |> printfn "expected 4, got %i"
// realPosL5 -7
// |> printfn "expected 3, got %i"
// realPosL5 10
// |> printfn "expected 0, got %i"
// realPosL5 5
// |> printfn "expected 0, got %i"
// realPosL5 0
// |> printfn "expected 0, got %i"
// realPosL5 4
// |> printfn "expected 4, got %i"
// realPosL5 7
// |> printfn "expected 2, got %i"

let prevPos (len: int) (pos: int) =
    realPos len (pos - 1)

let nextPos (len: int) (pos: int) =
    realPos len (pos + 1)

type State = {StartPos: int; SkipAcc: int; CircularList: int list;}

let mkCircularList (startPos: int) (endPos: int) (size: int) (circularList: int list) =
    let len = circularList.Length
    let nextPos = nextPos len
    let prevPos = prevPos len
    let rec loop (posA: int) (posB: int) (countDown: int) (xs: int list) =
        // printfn "countDown: %i" countDown
        if countDown = 0 then xs else
        match posA.CompareTo(posB) with
            | 0 ->
                xs
            | -1 ->
                // if posA-1 < 0 then
                //     printfn ">>> postA (%i) - 1 = %i" posA (posA-1)
                //     printfn "%A" xs.[0..posA-1]
                //     failwith "Bug 1"
                // if posB+1 >= len then
                //     printfn "%A" xs.[posB+1..len-1]
                //     failwith "Bug 2"
                // if posA+1 > posB-1 then
                //     printfn ">>> postA (%i) + 1 = %i" posA (posA+1)
                //     printfn ">>> postB (%i) - 1 = %i" posB (posB-1)
                //     printfn "%A" xs.[posA+1..posB-1]
                //     failwith "Bug 6"
                List.concat
                    [
                        xs.[0..posA-1]
                        [xs.[posB]]
                        xs.[posA+1..posB-1]
                        [xs.[posA]]
                        xs.[posB+1..len-1]
                    ]
                |> fun xs1 ->
                    if (nextPos posA).CompareTo(prevPos posB) = 1
                    then
                        printfn "%A" xs1
                        xs1
                    else
                        loop (nextPos posA) (prevPos posB) (countDown - 1) xs1
            | 1 ->
                // if posB-1 < 0 then failwith "Bug 3"
                // if posA+1 >= len then
                //     printfn ">>> postA (%i) + 1 = %i" posA (posA+1)
                //     printfn "%A" xs.[posA+1..len-1]
                //     failwith "Bug 4"
                // if posB+1 > posA-1 then
                //     printfn ">>> postB (%i) + 1 = %i" posB (posB+1)
                //     printfn ">>> postA (%i) - 1 = %i" posA (posA-1)
                //     printfn "%A" xs.[posB+1..posA-1]
                //     failwith "Bug 5"
                List.concat
                    [
                        xs.[0..posB-1]
                        [xs.[posA]]
                        xs.[posB+1..posA-1]
                        [xs.[posB]]
                        xs.[posA+1..len-1]
                    ]
                |> fun xs1 ->
                    if (nextPos posA).CompareTo(prevPos posB) = -1 && countDown < len
                    then xs1
                    else
                        loop (nextPos posA) (prevPos posB) (countDown - 1) xs1
            | _ -> failwith "Mission impossible !!"
    loop startPos endPos size circularList

let folder (state: State) (size: int) =
    let len = state.CircularList.Length
    let nextStartPos =
        (state.StartPos + state.SkipAcc + size)
        |> fun pos -> realPos len pos
    let endPos =
        (state.StartPos + size - 1)
        |> fun pos -> realPos len pos

    {state with
        CircularList = mkCircularList state.StartPos endPos size state.CircularList
        StartPos = nextStartPos
        SkipAcc = state.SkipAcc + 1}
    |> fun s -> printfn "%A" s; s

[<EntryPoint>]
let main _ =
    let state = {StartPos = 0; SkipAcc = 0; CircularList = circularExample;}
    inputExample
    // let state = {StartPos = 0; SkipAcc = 0; CircularList = circular;}
    // input
    |> List.fold (folder) state
    |> fun (s: State) -> s.CircularList
    |> List.take 2
    |> List.fold (*) 1
    |> printfn "%A"
    |> ignore

    0 // return an integer exit code
