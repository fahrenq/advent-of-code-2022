open System.IO

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

let pt1 =
  infiniteMovements (parseMovements INPUT)
  |> Seq.scan
    (fun (rows: int64[][], lastShape, lastShapeN) direction ->
      if rows.Length = 0 then
        let nextShapeN = lastShapeN + 1L
        let nextShape = getNextShape lastShape
        spawnShape rows nextShapeN (shapeToFn nextShape), nextShape, nextShapeN
      else
        let newRows, isMoved = moveShape lastShapeN direction rows

        match direction with
        | Left
        | Right -> newRows, lastShape, lastShapeN
        | Down ->
          if isMoved then
            newRows, lastShape, lastShapeN
          else
            let nextShapeN = lastShapeN + 1L
            let nextShape = getNextShape lastShape

            spawnShape newRows nextShapeN (shapeToFn nextShape),
            nextShape,
            nextShapeN
    )
    ([||], Square, EMPTY)
  |> Seq.find (fun (_, _, n) -> n = 2022L)
  |> (fun (rows, _, _) -> rows)
  |> Array.filter (fun row -> row |> Array.exists (fun x -> x <> EMPTY))
  |> Array.length



// |> Array.filter (fun row -> row |> Array.exists (fun x -> x <> EMPTY))
// |> Array.length
// |> printfn "%d"

// infiniteMovements (parseMovements INPUT)
// |> Seq.scan
//   (fun (rows: int64[][], lastShape, lastStoppedShapeN) direction ->
//     if rows.Length = 0 then
//       let nextShape = getNextShape lastShape
//       spawnShape rows (shapeToFn nextShape), nextShape, lastStoppedShapeN
//     else
//       let newRows, isMoved = moveLastShape direction rows
//
//       match direction with
//       | Left
//       | Right -> newRows, lastShape, lastStoppedShapeN
//       | Down ->
//         if isMoved then
//           newRows, lastShape, lastStoppedShapeN
//         else
//           let nextShape = getNextShape lastShape
//           spawnShape newRows (shapeToFn nextShape), nextShape, lastStoppedShapeN
//   )
//   ([||], Square, EMPTY)
// |> Seq.find (fun (_, _, n) -> n = 1000000000000L - 1L)
// // |> Seq.find (fun (_, _, n) -> n = 2022L - 1L)
// |> (fun (rows, _, _) -> rows)
// |> Array.filter (fun row -> row |> Array.exists (fun x -> x <> EMPTY))
// |> Array.length
// |> printfn "%d"
