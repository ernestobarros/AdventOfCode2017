let inputExample = [3; 4; 1; 5]

let input = [199; 0; 255; 136; 174; 254; 227; 16; 51; 85; 1; 2; 22; 17; 7; 192]

let logAndContinue = fun x -> printfn "%A" x; x

let mkMergeCircle (idx: int) (len: int) (frontPart: int list) (backPart: int list) (oldCircle: int list) =
    if frontPart.Length > 0 then
        List.concat
            [
                frontPart
                oldCircle.[frontPart.Length .. (idx - 1)]
                backPart
            ]
    else
        List.concat
            [
                oldCircle.[0 .. (idx - 1)]
                backPart
                oldCircle.[(idx + len) .. (oldCircle.Length - 1)]
            ]

let mkCircle (idx: int) (len: int) (circle: int list) =
    let virtualCircle = List.append circle circle
    let reversedRangeCircle =
        virtualCircle
        |> List.skip idx
        |> List.take len
        |> List.rev
    let (frontPart, backPart) =
        let hasOverflow =
            idx + reversedRangeCircle.Length > circle.Length
        if hasOverflow then
            let idxSplit = circle.Length - idx
            reversedRangeCircle
            |> List.splitAt idxSplit
            |> fun (backPart, frontPart) -> frontPart, backPart
        else [], reversedRangeCircle
    mkMergeCircle idx len frontPart backPart circle
    |> logAndContinue

type State =
    {
        IndexPos: int
        SkipAcc: int
        Circle: int list
    }

let step (state: State) (len: int) =
    let {IndexPos=idx; SkipAcc=skip; Circle=circle}: State = state
    {
        IndexPos = (idx + len + skip) % circle.Length
        SkipAcc = skip + 1
        Circle = mkCircle idx len circle
    }

[<EntryPoint>]
let main _ =
    // let initState = {IndexPos = 0; SkipAcc = 0; Circle = [0 .. 4]}
    // inputExample
    let initState = {IndexPos = 0; SkipAcc = 0; Circle = [0 .. 255]}
    input
    |> List.fold (step) initState
    |> logAndContinue
    |> fun s -> s.Circle
    |> List.take 2
    |> List.fold (*) 1
    |> printfn "%A"

    0 // return an integer exit code
