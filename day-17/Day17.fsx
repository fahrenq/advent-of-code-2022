#r "System.Security.Cryptography"

open System.IO
open System.Text

open System.Security.Cryptography


let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")
let TEST_INPUT = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

let WIDTH = 7
let EMPTY = 0L
let emptyRow = Array.create WIDTH EMPTY

let printRows (rows: int64[][]) =
  "\n"
  + (rows
     |> Array.map (fun row ->
       row |> Array.map (fun x -> if x = EMPTY then "." else x.ToString())
     )
     |> Array.map (String.concat "")
     |> String.concat "\n")

fsi.AddPrinter<int64[][]>(printRows)

let fillToWidth row = Array.append row (Array.create (WIDTH - row.Length) EMPTY)

type Direction =
  | Left
  | Right
  | Down

let lineShape n = Array.create 4 n |> Array.singleton

let plusShape n =
  [| [| EMPTY; n; EMPTY |]; [| n; n; n |]; [| EMPTY; n; EMPTY |] |]

let cornerShape n =
  [| [| EMPTY; EMPTY; n |]; [| EMPTY; EMPTY; n |]; [| n; n; n |] |]

let lShape n = Array.create 4 (Array.singleton n)

let squareShape n = Array.create 2 (Array.create 2 n)

type Shape =
  | Line
  | Plus
  | Corner
  | L
  | Square


let getNextShape =
  function
  | Line -> Plus
  | Plus -> Corner
  | Corner -> L
  | L -> Square
  | Square -> Line

let shapeToFn =
  function
  | Line -> lineShape
  | Plus -> plusShape
  | Corner -> cornerShape
  | L -> lShape
  | Square -> squareShape

// new shape appears -
// left edge 2 units away from the left border
// bottom edge 3 units above the highest rock in the room


let isBlocked (direction: Direction) (shapeN: int64) (row: int64[]) =
  match direction with
  | Left ->
    let firstIdx = row |> Array.findIndex (fun x -> x = shapeN)
    firstIdx = 0 || row[firstIdx - 1] <> EMPTY
  | Right ->
    let lastIdx = row |> Array.findIndexBack (fun x -> x = shapeN)
    lastIdx = (row.Length - 1) || row.[lastIdx + 1] <> EMPTY
  | Down -> failwithf "isBlocked: Down is not a valid direction"

let moveRow (direction: Direction) (shapeN: int64) (row: int64[]) =
  let rowCopy = Array.copy row
  let firstIdx = rowCopy |> Array.findIndex (fun x -> x = shapeN)
  let lastIdx = rowCopy |> Array.findIndexBack (fun x -> x = shapeN)

  match direction with
  | Left ->
    rowCopy[firstIdx - 1] <- shapeN
    rowCopy[lastIdx] <- EMPTY
  | Right ->
    rowCopy[firstIdx] <- EMPTY
    rowCopy[lastIdx + 1] <- shapeN
  | Down -> ()

  rowCopy

let rec moveShape shapeN (direction: Direction) (rows: int64[][]) =
  match direction with
  | Left
  | Right ->
    let linesWithShapeIds, linesWithShape =
      rows
      |> Array.indexed
      |> Array.filter (fun (i, row) -> row |> Array.contains shapeN)
      |> Array.unzip

    if not <| Array.exists (isBlocked direction shapeN) linesWithShape then
      rows
      |> Array.mapi (fun i row ->
        if linesWithShapeIds |> Array.contains i then
          moveRow direction shapeN row
        else
          row
      ),
      true
    else
      rows, false
  | Down ->
    let newRows, isMoved = rows |> Array.transpose |> moveShape shapeN Right
    newRows |> Array.transpose, isMoved


let getLastShapeN (rows: int64[][]) = rows |> Array.collect id |> Array.max

let moveLastShape (direction: Direction) (rows: int64[][]) =
  let lastShapeN = getLastShapeN rows
  moveShape lastShapeN direction rows

let spawnShape (rows: int64[][]) newShapeN makeShape =
  let shape = makeShape newShapeN |> Array.map fillToWidth

  // 3 empty lines above the highest rock or floor
  // offset by 2 units to the left

  // 1. trim all empty lines from the top
  let rowsWithRocks =
    rows |> Array.filter (fun row -> row |> Array.exists (fun x -> x <> EMPTY))

  // 2. Add 3 empty lines to the top
  let rowsWithRocksAndEmptyLines =
    rowsWithRocks |> Array.append (Array.create 3 emptyRow)

  let withRocks = rowsWithRocksAndEmptyLines |> Array.append shape

  withRocks
  |> (moveShape newShapeN Right >> fst)
  |> (moveShape newShapeN Right >> fst)


let parseMovements input =
  input
  |> Seq.choose (
    function
    | '>' -> Some Right
    | '<' -> Some Left
    | _ -> None
  )
  |> Seq.collect (fun x -> [ x; Down ])
  |> Seq.toArray

let infiniteMovements (movements: Direction[]) =
  Seq.initInfinite (fun i -> movements[i % movements.Length])

// let pt1 =
//   infiniteMovements (parseMovements INPUT)
//   |> Seq.scan
//     (fun (rows: int64[][], lastShape, lastShapeN) direction ->
//       if rows.Length = 0 then
//         let nextShapeN = lastShapeN + 1L
//         let nextShape = getNextShape lastShape
//         spawnShape rows nextShapeN (shapeToFn nextShape), nextShape, nextShapeN
//       else
//         let newRows, isMoved = moveShape lastShapeN direction rows
//
//         match direction with
//         | Left
//         | Right -> newRows, lastShape, lastShapeN
//         | Down ->
//           if isMoved then
//             newRows, lastShape, lastShapeN
//           else
//             let nextShapeN = lastShapeN + 1L
//             let nextShape = getNextShape lastShape
//
//             spawnShape newRows nextShapeN (shapeToFn nextShape),
//             nextShape,
//             nextShapeN
//     )
//     ([||], Square, EMPTY)
//   |> Seq.find (fun (_, _, n) -> n = 2022L)
//   |> (fun (rows, _, _) -> rows)
//   |> Array.filter (fun row -> row |> Array.exists (fun x -> x <> EMPTY))
//   |> Array.length
//
//


let notEmptyRows (rows: int64[][]) =
  rows |> Array.filter (fun row -> row |> Array.exists (fun x -> x <> EMPTY))

let mutable prevHeight = 0
let increases: ResizeArray<int> = ResizeArray<_>()

// let pt2 =
//   infiniteMovements (parseMovements INPUT)
//   |> Seq.scan
//     (fun (rows: int64[][], lastShape, lastShapeN) direction ->
//       if rows.Length = 0 then
//         let nextShapeN = lastShapeN + 1L
//         let nextShape = getNextShape lastShape
//         spawnShape rows nextShapeN (shapeToFn nextShape), nextShape, nextShapeN
//       else
//         let newRows, isMoved = moveShape lastShapeN direction rows
//
//         match direction with
//         | Left
//         | Right -> newRows, lastShape, lastShapeN
//         | Down ->
//           if isMoved then
//             newRows, lastShape, lastShapeN
//           else
//             let nextShapeN = lastShapeN + 1L
//
//             let newHeight = (newRows |> notEmptyRows |> Array.length)
//             let heightIncrease = newHeight - prevHeight
//             increases.Add heightIncrease
//             prevHeight <- newHeight
//
//             let nextShape = getNextShape lastShape
//
//             spawnShape newRows nextShapeN (shapeToFn nextShape),
//             nextShape,
//             nextShapeN
//     )
//     ([||], Square, EMPTY)
//   |> Seq.find (fun (_, _, n) -> n = 565L)
//   |> (fun (rows, _, _) -> rows)
//   |> Array.filter (fun row -> row |> Array.exists (fun x -> x <> EMPTY))
//   |> Array.length
//
let increasesLines = increases.ToArray() |> Array.map (fun x -> x.ToString())

// File.WriteAllLines("/tmp/increases.txt", increasesLines)

// loop starts at 565 chars >
// height 889

let preCycleStr =
  "1 2 3 2 0 0 0 2 0 2 1 2 2 0 2 1 2 2 2 2 1 3 2 2 0 1 2 3 0 2 1 3 3 2 0 0 2 0 0 2 0 2 3 2 0 1 3 2 4 2 0 0 0 1 0 1 3 3 0 0 1 3 2 4 0 1 3 2 4 2 1 3 2 2 0 1 2 3 2 2 1 3 3 4 0 1 3 3 2 0 1 3 3 0 0 1 2 2 2 0 1 3 2 2 0 0 3 0 4 0 1 3 0 4 0 0 1 2 0 0 1 3 2 2 0 1 3 3 4 0 1 2 3 0 1 1 2 3 0 1 1 2 3 0 0 0 1 3 0 1 1 2 3 2 2 1 2 1 3 0 0 2 2 2 0 0 2 2 1 2 1 3 2 2 0 1 2 3 0 2 1 3 2 2 0 1 3 0 4 2 1 3 3 2 0 1 3 3 0 2 1 3 3 2 2 1 3 2 0 0 1 2 1 0 1 1 3 3 0 0 0 2 3 0 0 1 2 3 0 0 1 3 3 2 0 1 3 3 0 0 1 3 2 0 0 1 3 3 0 0 0 2 3 0 0 1 2 3 4 0 1 3 3 2 0 1 3 2 0 0 1 1 2 2 2 0 0 3 0 0 0 2"

let cycleStr =
  "3 2 0 1 2 1 3 0 1 2 2 4 0 1 2 3 0 0 1 3 3 4 0 1 3 3 4 0 1 3 2 2 0 1 3 3 2 0 0 2 2 0 0 1 3 2 4 0 0 0 3 0 0 1 2 1 2 2 1 3 3 2 0 0 2 2 0 2 1 3 2 2 2 1 2 2 4 0 1 3 0 2 2 1 2 2 2 2 1 2 2 2 2 1 3 3 2 0 1 3 2 1 2 1 3 2 0 0 1 2 2 1 1 1 2 3 2 0 1 3 0 1 2 1 2 1 2 0 1 3 2 1 1 1 3 2 2 2 1 2 2 2 2 1 3 2 2 0 1 3 2 0 0 1 2 1 3 0 1 3 2 4 2 1 3 3 2 2 1 3 2 0 1 1 3 3 0 0 1 3 3 4 2 1 3 3 2 0 1 3 0 2 0 1 3 2 4 2 1 3 3 2 2 1 2 3 0 0 1 3 0 4 0 1 3 3 2 0 1 3 2 2 0 1 2 1 4 0 1 2 3 0 0 1 2 3 0 0 0 3 0 1 0 1 3 3 0 2 1 2 1 2 0 1 3 2 2 2 0 0 3 4 2 1 3 3 4 0 1 3 3 0 0 1 2 1 2 2 1 3 3 4 0 1 3 0 3 0 1 3 2 1 2 1 2 2 0 0 1 3 2 4 0 0 0 1 2 0 1 3 3 0 0 1 2 1 2 0 0 2 3 0 0 1 3 2 2 0 1 2 2 0 0 1 2 2 1 0 0 3 2 4 0 1 3 3 0 0 1 3 2 0 0 1 3 2 4 0 1 2 2 4 0 1 3 3 0 0 0 2 0 3 0 1 3 2 2 2 1 3 2 2 2 1 2 3 2 0 0 2 3 4 0 0 0 3 0 2 0 2 1 2 0 1 3 3 2 0 1 2 1 4 2 1 2 1 3 0 0 3 0 1 2 1 3 3 2 0 1 3 2 2 2 1 3 2 2 0 1 3 3 0 0 1 3 3 0 0 0 2 2 2 2 1 2 3 4 2 1 3 3 4 0 1 3 2 1 0 0 3 3 4 0 1 2 3 0 0 1 3 0 3 0 0 2 1 1 0 1 3 2 2 2 1 3 2 4 0 1 2 1 2 0 1 3 3 4 0 1 3 2 1 0 0 3 2 2 0 1 3 2 2 0 1 2 3 0 2 1 3 3 0 0 1 3 3 0 0 1 2 2 4 0 1 3 0 3 0 1 3 0 3 2 0 3 0 3 0 0 2 2 0 0 1 3 3 0 2 1 3 3 0 0 1 2 3 0 1 1 3 2 0 0 1 3 2 2 2 1 3 0 2 0 1 2 1 2 2 1 3 3 2 0 1 3 3 2 2 1 2 1 2 2 1 2 3 4 0 1 2 3 2 0 1 3 3 4 0 0 2 3 0 0 0 2 3 2 2 1 3 3 4 0 1 3 2 2 0 1 3 3 2 0 1 3 3 0 0 1 2 1 2 0 0 3 0 4 2 1 3 3 0 0 1 0 3 2 0 1 3 2 2 0 1 3 2 0 0 1 2 1 1 0 1 3 3 2 0 1 3 2 4 0 0 2 2 0 2 1 3 3 4 2 0 3 3 0 2 0 0 3 0 0 1 3 3 0 2 1 2 1 2 2 1 0 3 2 0 1 3 3 2 2 1 3 2 2 2 0 0 3 0 0 1 3 0 2 0 1 3 3 4 0 1 3 3 2 2 1 3 3 0 0 1 3 3 2 0 1 2 2 2 0 1 3 0 2 0 1 2 2 2 0 1 3 3 0 0 0 2 2 2 0 1 3 3 0 0 1 3 2 4 0 1 3 3 4 0 1 2 1 1 2 1 2 2 4 0 1 3 0 4 0 0 2 3 0 0 0 2 3 4 0 1 3 3 4 0 1 3 3 0 2 1 3 3 2 0 1 2 2 1 1 1 3 3 2 2 1 3 2 2 0 1 3 0 3 2 1 3 3 4 0 1 3 2 2 0 1 3 2 4 0 1 3 3 2 2 1 3 2 1 1 1 2 2 2 2 1 3 3 2 0 1 0 0 4 2 1 2 3 4 0 1 2 1 3 2 0 3 2 2 0 1 0 3 1 2 1 3 3 0 0 1 2 3 0 1 1 3 3 2 0 1 3 2 2 0 1 3 3 0 0 1 3 3 4 0 1 3 3 0 0 0 2 3 0 0 1 1 2 2 0 0 2 2 2 0 0 2 3 0 0 1 3 0 4 2 1 2 2 2 0 1 3 3 4 0 1 3 3 2 0 1 2 3 0 1 1 2 1 2 2 1 3 0 4 2 1 2 3 0 1 0 3 0 4 2 1 3 3 0 0 1 2 1 2 0 1 3 0 3 0 1 3 2 0 0 0 2 1 3 0 1 1 2 1 2 0 3 0 2 0 1 2 2 2 2 1 3 2 0 2 1 3 2 1 2 1 3 2 0 0 1 3 2 2 0 0 2 0 2 0 0 3 2 2 0 0 2 3 2 2 1 3 3 2 2 1 3 0 2 0 1 3 3 2 2 1 3 2 2 2 1 3 3 2 0 1 2 3 0 1 0 3 2 2 0 0 2 3 0 0 1 3 3 4 0 1 3 2 2 0 1 3 3 0 0 1 3 3 4 0 1 2 3 2 2 1 3 3 0 0 1 2 3 0 0 1 3 2 0 0 1 3 0 1 1 1 3 2 2 0 1 3 3 4 2 1 3 3 2 2 1 2 2 0 0 1 3 3 0 0 1 3 2 2 2 0 3 0 0 2 0 2 3 2 0 0 2 3 0 2 1 3 0 4 0 1 3 3 0 2 0 3 0 4 0 1 3 2 2 0 1 3 3 4 2 1 3 3 4 2 1 3 0 2 2 0 0 3 2 2 1 3 3 0 0 1 3 0 2 0 1 2 2 0 0 1 3 3 4 2 0 3 0 0 2 1 3 2 4 0 1 3 2 0 2 1 3 3 0 2 0 2 1 2 0 1 3 2 2 0 1 3 2 0 0 1 3 3 2 2 1 2 2 2 0 1 0 3 2 2 1 2 2 0 2 1 3 2 2 0 1 2 1 2 0 1 3 2 0 0 0 3 3 0 0 1 3 3 2 0 1 2 2 0 0 1 3 2 4 0 0 2 2 2 0 1 2 3 0 2 1 2 1 2 0 1 3 3 2 0 0 2 2 2 0 1 3 3 4 2 0 0 3 0 0 0 2 0 3 0 1 3 3 0 0 1 3 2 4 2 1 3 2 0 2 1 3 2 2 0 1 3 3 2 0 1 3 3 0 0 1 2 3 2 0 1 3 0 3 0 1 2 3 0 0 1 2 1 2 0 1 2 2 0 0 1 3 0 4 2 1 2 3 2 0 1 2 3 0 0 1 3 3 4 2 1 3 2 2 0 1 3 0 1 0 1 2 3 0 0 0 3 3 0 0 1 3 0 3 0 0 2 1 2 2 1 3 2 0 0 1 1 3 0 2 0 2 3 4 0 1 3 2 2 0 1 3 2 0 0 0 2 3 2 0 1 2 3 2 0 1 3 2 0 2 1 2 2 2 2 1 3 3 0 0 1 3 3 0 0 1 3 3 4 2 1 3 3 0 0 1 0 3 2 2 1 2 2 2 0 0 2 1 4 0 0 1 2 2 0 0 2 1 4 0 1 2 3 0 0 1 3 3 2 0 1 1 2 2 2 1 3 2 2 0 1 3 3 0 0 1 3 3 2 2 1 3 2 2 0 0 2 2 2 0 1 3 2 2 0 1 3 2 0 0 1 2 1 2 2 0 0 3 0 0 1 3 3 4 0 1 3 3 0 0 0 1 3 2 0 1 3 3 4 0 0 2 3 2 0 1 3 2 2 0 0 2 3 2 0 1 3 3 4 2 1 3 0 3 0 0 3 0 2 0 1 3 3 4 0 1 3"

let preCycles = preCycleStr.Split " " |> Array.map int
let cycles = cycleStr.Split " " |> Array.map int

let cycleShapes = cycles.LongLength
let preCyclesShapes = preCycles.LongLength
let preCycleHeight = preCycles |> Array.sum |> int64
let cycleHeight = cycles |> Array.sum |> int64

let GOAL_SHAPES = 1000000000000L
let fullCyclesInGoal = (GOAL_SHAPES - preCyclesShapes) / cycleShapes


// now find our position after pre + full cycles
let posSoFar = preCyclesShapes + (cycleShapes * fullCyclesInGoal)

let remainingShapes = GOAL_SHAPES - posSoFar

let remainingHeight =
  cycles |> Array.take (int remainingShapes) |> Array.sum |> int64

let answer = preCycleHeight + (cycleHeight * fullCyclesInGoal) + remainingHeight


(*
Holy shit this was chaotic and not pretty.
I solved it by selecting pattern in VS Code and looking where it highlights
repetitions.

LINE BELOW WAS SUGGESTED BY COPILOT:
I mean, I might not be the best programmer, but I'm not a complete idiot.
*)
