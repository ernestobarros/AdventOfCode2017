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

type Prog = {Id: int; Neighbours: int list}

let deserializeProgs (input: string) =
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
        {
            Id = id
            Neighbours = neighbours
        }
    )

let mkConnectedGroup (itemInGroup: int) (progs: Prog list) =
    let genConnected (connected: int list) =
        if connected.IsEmpty then None else
        let newConnected =
            [
                connected
                progs |> List.filter (fun prog ->
                    prog.Neighbours
                    |> List.exists(fun neighbourId ->
                        connected |> List.contains neighbourId
                    )
                ) |> List.map (fun prog -> prog.Id)
            ]
            |> List.concat
            |> List.distinct
        if connected = newConnected
        then Some(connected, [])
        else Some(connected, newConnected)
    List.unfold genConnected [itemInGroup]
    |> List.concat


let mkConnectedGroups (progs: Prog list) =
    let setOfGroups (groups: (int Set) Set) =
            groups
            |> Set.map (Set.toList)
            |> Set.toList
            |> List.concat
            |> Set.ofList
    let setOfProgs (progs: Prog list) =
            progs
            |> List.map (fun prog -> prog.Id)
            |> Set.ofList
    let isExhausted (groups: (int Set) Set) =
        let fstSet = setOfProgs progs
        let sndSet = setOfGroups groups
        Set.difference fstSet sndSet |> (Set.count >> (=) 0)
    let findFirstNotInGroups (groups: (int Set) Set) =
        let fstSet = setOfProgs progs
        let sndSet = setOfGroups groups
        Set.difference fstSet sndSet |> Set.toList |> List.head
    let mkNextGroup groups =
        mkConnectedGroup
            (findFirstNotInGroups groups)
            progs
        |> Set.ofList
    let genGroups (groups: (int Set) Set) =
        if groups.IsEmpty then None else
        if isExhausted groups
        then
            Some(groups, Set.ofList [])
        else
            let newGroup : int Set =
                mkNextGroup groups
            let newGroups : (int Set) Set =
                newGroup :: (Set.toList groups)
                |> Set.ofList
            Some(groups, newGroups)

    let groups : (int Set) Set =
        Set.ofList [mkNextGroup (Set.ofList [])]
    List.unfold genGroups groups
    |> List.last

let countConnectedTo (progId: int) (groups: (int Set) Set) =
    groups
    |> Set.filter (fun group -> Set.contains progId group)
    |> Set.unionMany
    |> Set.count

[<EntryPoint>]
let main _ =
    // exampleInput
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> deserializeProgs
    |> mkConnectedGroups
    // |> countConnectedTo 0
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