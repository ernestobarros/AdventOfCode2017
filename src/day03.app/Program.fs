type Direction =
    | R // Right
    | U // Up
    | L // Left
    | D // Down


let sumAdjacentSquares (x, y) spiral =
    let adjacentSquares =
        [
            for x' in -1 .. 1 do
            for y' in -1 .. 1 do
            yield (x + x', y + y')
        ] |> List.filter(fun (x', y') -> (x', y') <> (x, y))

    spiral
    |> List.filter(fun (_, _, (x', y'), _, _) -> adjacentSquares |> List.contains(x', y'))
    |> List.sumBy(fun (_, v, _, _, _) -> v)


let growSpiral spiral =
    match spiral with
    | [] ->  []
    | (i, v, (x, y), d, ws) :: _' ->
        let i' = i + 1
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
            if i' = List.sum(ws) + 1 then
                match d with
                    | R -> U
                    | U -> L
                    | L -> D
                    | D -> R
            else d
        let ws' =
            // Change of direction, Turn point reached end of the road
            if i' = List.sum(ws) + 1 then
                match d' with
                    | L
                    | R -> (ws.Head + 1) :: [List.sum(ws)]
                    | _ -> ws.Head :: [List.sum(ws)]
            else ws
        let v' = sumAdjacentSquares (x', y') spiral
        // Return the new spiral
        (i', v', (x', y'), d', ws') :: spiral


let headValueSpiral spiral =
    match spiral with
    | [] ->  0
    | (_, v, (_, _), _, _) :: _' -> v


let mkSpiral thresholdValue =
    let rec loop spiral =
        if (headValueSpiral spiral) > thresholdValue then List.rev spiral
        else loop (growSpiral spiral)
    loop [1, 1, (0, 0), R, [1]]


let printSpiral thresholdValue =
    mkSpiral thresholdValue
    |> List.last |> (printfn "%A")


[<EntryPoint>]
let main _ =
    printSpiral (265149)
    0 // return an integer exit code

(*
147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806--->   ...
*)
