open System
open System.IO
open System.Text.RegularExpressions

type Direction =
  | NorthEast
  | SoutEast
  | South
  | SouthWest
  | NorthWest
  | North

let toDirection =
  function
  | "ne" -> NorthEast
  | "se" -> SoutEast
  | "s"  -> South
  | "sw" -> SouthWest
  | "nw" -> NorthWest
  | "n"  -> North
  | unknown -> failwithf "Unkown direction `%s` !!" unknown

type State = {Pos: int*int; MostStepsAway: int}

let calcSteps (x: int, y: int) =
  List.max [(abs x); (abs y)]
  |> float
  |> fun x -> Math.Round(x / 2.0, MidpointRounding.AwayFromZero)
  |> int

let folder {Pos=(x:int, y:int); MostStepsAway=maxSteps} (d: Direction) =
  let newPos =
    match d with
    | NorthEast -> (x+2, y+1)
    | SoutEast  -> (x+2, y-1)
    | South     -> (x+0, y-2)
    | SouthWest -> (x-2, y-1)
    | NorthWest -> (x-2, y+1)
    | North     -> (x+0, y+2)
  {Pos=newPos; MostStepsAway=List.max [(calcSteps newPos); maxSteps]}

let endPos (input: string) =
  let initState = {Pos=(0,0); MostStepsAway=0}
  input.Trim().Split(',')
  |> Seq.toList
  |> List.map (fun x -> Regex.Replace(x.Trim(), "\s", ""))
  |> List.map (toDirection)
  |> List.fold (folder) initState

let test (input: string, expected: string) =
  let position = (endPos input)
  printfn "%15s %8s %s %A"
    <| input 
    <| sprintf "%A" position 
    <| expected
    <| sprintf "Got %i steps away !!" (calcSteps position.Pos)

("ne,ne,ne", "is 3 steps away.")
|> test
("ne,ne,sw,sw", "is 0 steps away (back where you started).")
|> test
("ne,ne,s,s", "is 2 steps away (se,se).")
|> test
("se,sw,se,sw,sw", "is 3 steps away (s,s,sw).")
|> test

File.ReadAllText("./input.txt")
|> endPos
|> fun s -> printfn "Most steps away is %i" s.MostStepsAway; s.Pos
|> calcSteps
|> printfn "Answer: %i"



(*
For example:

ne,ne,ne is 3 steps away.
ne,ne,sw,sw is 0 steps away (back where you started).
ne,ne,s,s is 2 steps away (se,se).
se,sw,se,sw,sw is 3 steps away (s,s,sw)
*)

(*
open System.Text.RegularExpressions

"ne,ne,ne"
|> fun x -> Regex.Replace(x, ",", ";")
|> printfn "%A"
*)
 
