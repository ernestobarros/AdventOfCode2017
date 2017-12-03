type Direction =
    | R // Right
    | U // Up
    | L // Left
    | D // Down

// seq<int * (int * int) * Direction * int list>
let rec spiral : seq<int * (int * int) * Direction * int list> = seq {
  yield! [1, (0, 0), R, [1]]
  yield!
    spiral
    |> Seq.map(fun (v, (x, y), d, ws) ->
        let v' = v + 1
        // x': new x position
        let x' =
            // d: previous direction
            match d with
                | R -> x + 1
                | L -> x - 1
                | _ -> x
        // y': new y position
        let y' =
            // d: previous direction
            match d with
                | U -> y + 1
                | D -> y - 1
                | _ -> y

        let d' =
            // Turn point reached end of the road
            if v' = List.sum(ws) + 1 then
                match d with
                    | R -> U
                    | U -> L
                    | L -> D
                    | D -> R
            else d
        let ws' =
            // Change of direction, Turn point reached end of the road
            if v' = List.sum(ws) + 1 then
                match d' with
                    | L
                    | R -> ws.Head + 1 :: ws
                    | _ -> ws.Head :: ws
            else ws
        let squareInfo = (v', (x', y'), d', ws')
        squareInfo
    )
}

let manhattenDistance square =
    let countOfSquares = square
    spiral
    |> Seq.take countOfSquares
    |> List.ofSeq
    |> Seq.find(fun (x, _, _, _) -> x = square)
    |> fun (_, (x, y), _, _) -> abs x + abs y
    |> (printfn "Distance between Square %i and Square 1 = %i" square)

[<EntryPoint>]
let main _ =
    // manhattenDistance 1
    // manhattenDistance 12
    // manhattenDistance 23
    // manhattenDistance 1024
    // manhattenDistance (1024 * 6)
    manhattenDistance 265149


    0 // return an integer exit code

(*
17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

For example:
Data from square 1 is carried 0 steps, since it's at the access port.
Data from square 12 is carried 3 steps, such as: down, left, left.
Data from square 23 is carried only 2 steps: up twice.
Data from square 1024 must be carried 31 steps.

*)


(*
Compiler issues ??

D:\work\fsharp\adventofcode\src\day03.app\Program.fs(11,5): warning FS0040:
This and other recursive references to the object(s) being defined will be checked for initialization-soundness at runtime through the use of a delayed reference.
This is because you are defining one or more recursive objects, rather than recursive functions.
This warning may be suppressed by using '#nowarn "40"' or '--nowarn:40'. [D:\work\fsharp\adventofcode\src\day03.app\day03.app.fsproj]

Process is terminating due to StackOverflowException.
*)