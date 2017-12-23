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
    // |> logAndContinue

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

let toLengthsFromASCII (input: string) =
    let hashingRounds = 64
    let data =
        List.concat
            [
                input |> Seq.map(int) |> List.ofSeq // to ASCII
                [17; 31; 73; 47; 23] // Nounce (arbitrary number that can only be used once)
            ]
    List.collect (fun _ -> data) [1 .. hashingRounds]
    |> logAndContinue

let toSparseHash input =
    let initState = {IndexPos = 0; SkipAcc = 0; Circle = [0 .. 255]}
    input
    |> toLengthsFromASCII
    |> List.fold (step) initState

let toDenseHash (sparseHash: int list) =
    let mapping = List.fold (^^^) 0
    sparseHash
    |> List.chunkBySize 16
    |> List.map (mapping)

// [65;27;9;1;4;3;40;50;91;7;6;0;2;5;68;22]
// |> List.fold (^^^) 0
// |> printfn "expected 64, got %A"


let toHex (denseHash: int list) =
    denseHash
    |> List.map ((sprintf "%x") >> (fun x -> if x.Length = 1 then "0"+x else x))
    |> List.fold (+) ""

[<EntryPoint>]
let main _ =
    // ""
    // "AoC 2017"
    // "1,2,3"
    // "1,2,4"
    "199,0,255,136,174,254,227,16,51,85,1,2,22,17,7,192"
    |> toSparseHash
    |> logAndContinue
    |> fun s -> s.Circle
    |> toDenseHash
    |> toHex
    |> printfn "%A"

    0 // return an integer exit code
