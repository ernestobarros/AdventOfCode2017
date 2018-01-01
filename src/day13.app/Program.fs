open System
open System.IO

let inputExample =
    [|
    0, 3
    1, 2
    4, 4
    6, 4
    |]
    |> Map.ofSeq

let input =
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
        .Trim()
        .Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun line ->
        let arr =
            line.Split([|':'|])
            |> Seq.map (fun s -> s.Trim() |> int)
            |> Seq.toArray
        arr.[0], arr.[1]
    )
    |> Map.ofSeq

type Direction = North | South
type Scanner = {Pos: int; Direction: Direction}
type Layer = {Scanner: Scanner; Range: int;}
type Depth = Layer of Layer | Empty
type Firewall = {Depths: Depth list}
type PacketState = {Pos: int; Penalty: int; Firewall: Firewall}

let toInitialPacketState (input: Map<int, int>) =
    let mkLayer range =
        {
            Scanner = { Pos = 0; Direction = South }
            Range = range
        }
    let mkDepth depth =
        match input.TryFind depth with
        | Some range -> Layer (mkLayer range)
        | None -> Empty
    let firewall =
        {
            Depths =
                [0 .. input |> Map.toList |> List.map fst |> List.max]
                |> List.map (mkDepth)
        }
    // By defenition: Penalty always 0 at Pos 0
    { Pos = 0; Penalty = 0; Firewall = firewall }

let mkStepScanner (range: int) (scanner: Scanner) =
    match scanner.Direction with
    | South ->
        let newPos = scanner.Pos + 1
        let newDirection =
            if newPos = range - 1
            then North
            else South
        { Pos = newPos; Direction = newDirection}
    | North ->
        let newPos = scanner.Pos - 1
        let newDirection =
            if newPos = 0
            then South
            else North
        { Pos = newPos; Direction = newDirection}

let mkStepDepth (depth: Depth) =
    match depth with
    | Empty
        -> Empty
    | Layer {Scanner=scanner; Range=range}
        -> Layer
            {
                Scanner = mkStepScanner range scanner
                Range = range
            }

let mkStepFirewall (firewall: Firewall) =
    { firewall with
        Depths =
            firewall.Depths
            |> List.map (mkStepDepth)
    }

let mkPenalty (pos: int) (firewall: Firewall) =
    if pos = firewall.Depths.Length then 0 else
    match firewall.Depths.[pos] with
    | Empty
        -> 0
    | Layer {Scanner=scanner; Range=range}
        ->
            if scanner.Pos = 0
            then pos * range
            else 0


let mkStepPacket (state: PacketState) =
    if state.Pos = state.Firewall.Depths.Length then None else
    let newPos = state.Pos + 1
    let newFirewall = mkStepFirewall state.Firewall
    let newPenalty = mkPenalty newPos newFirewall
    let newState =
        { state with
            Pos = newPos
            Penalty = newPenalty
            Firewall = newFirewall
        }
    Some(state, newState)

[<EntryPoint>]
let main _ =
    // inputExample
    input
    |> toInitialPacketState // The initial packet state
    |> List.unfold (mkStepPacket)
    |> List.sumBy (fun p -> p.Penalty)
    // |> List.map (fun p -> p.Firewall.Depths.[6])
    // |> List.map (fun (Layer {Scanner=scanner; Range=_}) -> scanner.Pos)
    |> printfn "%A"
    0 // return an integer exit code
