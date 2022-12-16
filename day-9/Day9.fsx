open System
open System.IO

let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

type Direction =
  | Up
  | Down
  | Left
  | Right

let gravitate a b =
  if (a - b) > 1 then b + 1
  elif (a - b) < -1 then b - 1
  else b

let follow (headX, headY) (tailX, tailY) =
  if (abs (headX - tailX) > 1) && (abs (headY - tailY) > 1) then
    let newX = gravitate headX tailX
    let newY = gravitate headY tailY
    newX, newY
  else
    if (abs (headX - tailX) > 1) then
      let newX = gravitate headX tailX
      newX, headY
    elif (abs (headY - tailY) > 1) then
      let newY = gravitate headY tailY
      headX, newY
    else
      tailX, tailY


let move direction (x, y) =
  match direction with
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Left -> x - 1, y
  | Right -> x + 1, y

let parseInputRow (row: string) =
  let tokens = row.Split(" ")
  let direction =
    match tokens[0] with
    | "U" -> Up
    | "D" -> Down
    | "L" -> Left
    | "R" -> Right
    | _ -> failwith "Invalid direction"

  Array.init (int tokens[1]) (fun _ -> direction)

let parseInput (input: string) =
  input.Split("\n", StringSplitOptions.RemoveEmptyEntries) |> Array.collect parseInputRow |> Array.toList

let TEST_INPUT = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

let TEST_MOVES = parseInput TEST_INPUT

let MOVES  = parseInput INPUT

let rec followHead head (tail: (int*int) list) acc =
  match tail with
  | [] -> acc
  | tailHead :: tailTail ->
    let newHead = follow head tailHead
    followHead newHead tailTail (newHead :: acc)

let printCoordinates (coordinates: (int*int) list) =
  let lines =
    Array.init 50 (fun y ->
      Array.init 50 (fun x ->
        match List.tryFindIndex (fun (x', y') -> (x-25) = x' && (y-25) = y') coordinates with
        | Some index -> string index
        | None -> "."
      ) |> String.concat ""
    ) |> String.concat "\n"

  printfn "%s" lines

let solve moves len =
  moves
  |> List.scan (fun coordinates direction ->
    let head = move direction (coordinates |> List.head)
    let tail = followHead head (coordinates |> List.tail) [] |> List.rev
    let newCoordinates = head :: tail
    // printfn "---"
    // printfn "direction: %A" direction
    // printCoordinates newCoordinates
    newCoordinates

  ) (List.init len (fun _ -> (0,0)))
  |> List.map List.last
  |> List.distinct
  |> List.length

let tailPositionsCount = solve MOVES 10
let tailPositionsCount' = solve MOVES 2

(*
For way too long I didn't realise that I need to implement diagonal moves
for part 2. And it was in the description, which I successfully ignored.
*)
