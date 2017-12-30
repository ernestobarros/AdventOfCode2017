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

let countConnectedTo (targetProgId: int) (progs: Prog list) =
    let generatorConnected (generatedState: int list) =
        if generatedState.IsEmpty then None else
        let newGeneratedState =
            progs
            |> List.filter(fun (prog: Prog) ->
                prog.Neighbours
                |> List.exists(fun neighbourId ->
                    generatedState |> List.contains neighbourId
                )
            )
            |> List.map(fun (prog: Prog) -> prog.Id)
        if
            generatedState = newGeneratedState
        then
            Some(generatedState, [])
        else
            Some(generatedState, newGeneratedState)
    let connected =
        let initialState = [targetProgId]
        List.unfold generatorConnected initialState
        |> List.concat
        |> List.distinct
        // |> logAndConinue
    let isConnected (prog: Prog) =
        List.contains prog.Id connected
    progs
    |> List.filter(isConnected)
    |> List.length

[<EntryPoint>]
let main _ =
    // exampleInput
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> deserializeProgs
    // |> List.map(logAndConinue)
    |> countConnectedTo 0
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