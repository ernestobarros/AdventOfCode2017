open System
open System.Text.RegularExpressions
open System.IO

let inputExample =
    """
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
    """

let (|Eq|Ne|Gt|Ge|Lt|Le|) operator =
    match operator with
    | "==" -> Eq
    | "!=" -> Ne
    | ">" ->  Gt
    | ">=" -> Ge
    | "<" ->  Lt
    | "<=" -> Le
    | _ -> failwith <| sprintf "Unknown operator %s !!" operator

let parseLine (line: string) =
    line.Trim()
    |> fun l ->
    let keyI = Regex.Match(l, "^\w+").Value
    let keyP = Regex.Match(l, "if\s+(\w+)").Groups.[1].Value
    let funcI =
        Regex.Match(l, "(dec|inc)\s+([-]?\d+)").Groups
        |> fun g ->
            let c =
                if g.[1].Value = "inc"
                then g.[2].Value |> int
                else g.[2].Value |> int |> (*) -1
            (fun x -> x + c)
    let funcP =
        let c = Regex.Match(l, "[-]?\d+$").Value |> int
        match Regex.Match(l, "([<>=!]+)\s+[-]?\d+$").Groups.[1].Value with
        | Eq -> fun x -> x = c
        | Ne -> fun x -> x <> c
        | Gt -> fun x -> x > c
        | Ge -> fun x -> x >= c
        | Lt -> fun x -> x < c
        | Le -> fun x -> x <= c
    (keyI, funcI), (keyP, funcP)

type AddFunc = int -> int
type PredicateFunc = int -> bool
type Instruction =
    (string * AddFunc) * (string * PredicateFunc)

let instructions =
    [
        ("b", fun x -> x + 5), ("a", fun x -> x > 1)
        ("a", fun x -> x + 1), ("b", fun x -> x < 5)
        ("c", fun x -> x + 10), ("a", fun x -> x >= 1)
        ("c", fun x -> x + -20), ("c", fun x -> x = 10)
    ]

let parseInput (input: string) =
    input.Trim().Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(parseLine)
    |> List.ofArray

let processInstruction (state: Map<string,int>) (instruction: Instruction) =
    let (keyAddInput: string, addFunc: AddFunc) =
        fst instruction
    let (keyPredicateInput: string, predicateFunc: PredicateFunc) =
        snd instruction
    state
    |> fun s -> if Map.containsKey keyAddInput s then s else Map.add keyAddInput 0 s
    |> fun s -> if Map.containsKey keyPredicateInput s then s else Map.add keyPredicateInput 0 s
    |> fun s ->
        let predicateInput = Map.find keyPredicateInput s
        let addInput = Map.find keyAddInput s
        if predicateFunc predicateInput |> not then s
        else Map.add keyAddInput (addFunc addInput) s

[<EntryPoint>]
let main _ =
    // inputExample
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))
    |> parseInput
    |> List.fold (processInstruction) Map.empty
    |> Map.toList
    |> List.maxBy(snd)
    |> (printfn "%A")
    0 // return an integer exit code

(*
Each instruction consists of several parts: the register to modify, whether to increase or decrease that register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:

b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
These instructions would be processed as follows:

Because a starts at 0, it is not greater than 1, and so b is not modified.
a is increased by 1 (to 1) because b is less than 5 (it is 0).
c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
c is increased by -20 (to -10) because c is equal to 10.
After this process, the largest value in any register is 1.
*)
