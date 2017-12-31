open System.Text.RegularExpressions
open System
open System.IO

let logAndConinue anyData = printfn "%A" anyData; anyData

let exampleInput =
    """
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
    """

let deserializeGraph (input: string) =
    input.Trim().Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
    |> List.ofSeq
    |> List.map(fun line ->
        let id, neighbours =
            Regex.Split(line.Trim(), "\s*<->\s*")
            |> List.ofSeq
            |> List.take 2
            |> fun x ->
                x.[0] |> int,
                Regex.Split(x.[1].Trim(), "\s*,\s*")
                |> List.ofSeq
                |> List.map(int)

        id, neighbours
    )
    |> Map.ofList

let neighboursOfNeighbours (target: int) (graph: Map<int, int list>) =
    let rec loop (v: int) (seen: int Set) =
        match Map.tryFind v graph with
        | Some values ->
            let newSeen = Set.union (Set.ofList values) seen
            values
            |> List.filter (fun v -> not <| Set.contains v seen)
            |> List.map (fun v -> loop v newSeen)
            |> List.fold (Set.union) seen
        | None -> seen
    let seen = Set.singleton target
    loop target seen

let groupsOfNeighboursOfNeighbours (graph: Map<int, int list>) =
    List.fold
        (fun groups target -> Set.add (neighboursOfNeighbours target graph) groups)
        Set.empty
        (List.map fst <| Map.toList graph)

[<EntryPoint>]
let main _ =
    // exampleInput
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> deserializeGraph
    |> groupsOfNeighboursOfNeighbours
    // |> neighboursOfNeighbours 0
    |> Set.count
    |> logAndConinue
    |> ignore
    0 // return an integer exit code

(*
In this example, the following programs are in the group that contains program ID 0:

Program 0 by definition.
Program 2, directly connected to program 0.
Program 3 via program 2.
Program 4 via program 2.
Program 5 via programs 6, then 4, then 2.
Program 6 via programs 4, then 2.
Therefore, a total of 6 programs are in this group; all but program 1,
which has a pipe that connects it to itself.

How many programs are in the group that contains program ID 0?
*)