type Direction =
    | R // Right
    | U // Up
    | L // Left
    | D // Down

let growSpiral spiral =
    match spiral with
    | [] ->  []
    | (v, (x, y), d, ws) :: _' ->
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
                    | R -> (ws.Head + 1) :: [List.sum(ws)]
                    | _ -> ws.Head :: [List.sum(ws)]
            else ws
        // Return the new spiral
        (v', (x', y'), d', ws') :: spiral


let mkSpiral size =
    let rec loop n spiral =
        if n = size then List.rev spiral
        else loop (n + 1) (growSpiral spiral)
    loop 1 [1, (0, 0), R, [1]]

let manhattenDistance square =
    let size = square
    mkSpiral size
    |> List.find(fun (x, _, _, _) -> x = square)
    |> fun (_, (x, y), _, _) -> abs x + abs y
    |> (printfn "Distance between Square %i and Square 1 = %i" square)

[<EntryPoint>]
let main _ =
    manhattenDistance 1
    manhattenDistance 12
    manhattenDistance 23
    manhattenDistance 1024
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
