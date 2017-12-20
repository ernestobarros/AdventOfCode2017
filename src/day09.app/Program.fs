open System.Text.RegularExpressions
open System
open System.IO

let inputExample =
    """
{},
{{{}}},
{{},{}},
{{{},{},{{}}}},
{<a>,<a>,<a>,<a>},
{{<ab>},{<ab>},{<ab>},{<ab>}},
{{<!!>},{<!!>},{<!!>},{<!!>}},
{{<a!>},{<a!>},{<a!>},{<ab>}},
    """
//     """
// {}{{{{{{{{{{{{}}}}}}}}}}}}
//     """

let logAndContinue x = printfn "%A" x; x

let cleanGarbage (input: string) =
    let result1 =
        input.Trim()
        |> fun x -> x.Replace(Environment.NewLine, "")
        // |> logAndContinue
        |> fun x -> Regex.Replace(x, "!.", "")
        // |> logAndContinue
    let matchGarbage = "<[^>]*?>"
    let matchesGarbage: MatchCollection = Regex.Matches(result1, matchGarbage)
    printfn "********************"
    matchesGarbage
    |> Seq.toList
    |> List.sumBy(fun x -> x.Value.Length - 2)
    |> logAndContinue
    |> ignore
    printfn "********************"

    result1
    |> fun x -> Regex.Replace(x, matchGarbage, "")
    // |> logAndContinue
    |> fun x -> x.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
    |> fun x -> String.Join(',', x)
    |> fun x -> x.Replace(",", "")

let mkScore (data: string) =
    let folder1 (state: string) (a: string, b: string) =
        state.Replace(a, b)
    let folder2 (state1: string) (a: string, b: string) =
        let state2 = Regex.Replace(state1, "(\d+)", "`$1`")
        let pattern = sprintf "`%s`;" a
        let replacement = sprintf "%s;" b
        Regex.Replace(state2, pattern, replacement)
        |> fun x -> x.Replace("`", "")

    let rec loop (data: string)  =
        let isFinished = Regex.Match(data, "\{") |> fun x1 -> x1.Success |> not
        if isFinished then data
        else
            let pattern = "\{\d+;(\d+;)*\}"
            let replacePair1 =
                Regex.Matches(data, pattern)
                |> Seq.map(fun m1 -> m1.Value)
                |> Set.ofSeq
                |> Set.map(fun x1 ->
                    let replacePair2 =
                        Regex.Matches(x1, "\d+")
                        |> Seq.map(fun x2 -> x2.Value |> int)
                        |> Set.ofSeq
                        |> Set.toList
                        // |> fun z -> z |> List.map(string >> (+) " ") |> List.fold (+) "" |> (printfn "%A"); z
                        |> List.map(fun x3 -> x3, (+) 1 x3)
                        |> List.sortDescending
                        |> List.map(fun (a, b) -> (a |> string), (b |> string))
                    List.fold (folder2) x1 replacePair2
                    |> fun x4 -> x4.Replace("{2", "1;2")
                    |> fun x5 -> x1, Regex.Replace(x5, "(\d+;)}", "$1")
                )
                |> Set.toList
                // |> logAndContinue
            List.fold (folder1) data replacePair1
            // |> logAndContinue
            |> loop

    data.Replace("{}", "1;")
    // |> logAndContinue
    |> loop

let parseInput (cleanInput: string) =
    // cleanInput.Substring(0,8)
    // cleanInput.Substring(0,14)
    // cleanInput.Substring(0,26)
    // cleanInput.Substring(0,28)
    // cleanInput.Substring(0,38)
    // cleanInput.Substring(0,42)
    cleanInput
    // |> logAndContinue
    |> mkScore
    |> fun x -> x.Split([|';'|], StringSplitOptions.RemoveEmptyEntries)

[<EntryPoint>]
let main _ =
    // inputExample
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> cleanGarbage
    // |> logAndContinue
    |> parseInput
    |> Seq.sumBy(int)
    |> printfn "%A"
    0 // return an integer exit code

(*
Your goal is to find the total score for all groups in your input.
Each group is assigned a score which is one more than the score of the group that immediately contains it.
(The outermost group gets a score of 1.)

{}, score of 1.
{{{}}}, score of 1 + 2 + 3 = 6.
{{},{}}, score of 1 + 2 + 2 = 5.
{{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
{<a>,<a>,<a>,<a>}, score of 1.
{{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
{{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
{{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
What is the total score for all groups in your input?
50
*)
