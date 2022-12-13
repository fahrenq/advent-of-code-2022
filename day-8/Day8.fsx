open System
open System.IO

let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

let TEST_INPUT =
  """30373
25512
65332
33549
35390"""

let getMatrix (input: string) =
  input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
  |> Array.map (fun r -> r.ToCharArray() |> Array.map (string >> Int32.Parse))

module Part1 =
  let visibleOneDirection row =
    row |> Array.mapFold (fun acc x -> x > acc, max x acc) Int32.MinValue |> fst

  let visibleBothDirections row =
    let d1 = visibleOneDirection row
    let d2 = visibleOneDirection (Array.rev row) |> Array.rev
    Array.map2 (||) d1 d2

  let solve (input: string) =
    let m = getMatrix input
    let visibleHorizontal = m |> Array.map visibleBothDirections

    let visibleVertical =
      m |> Array.transpose |> Array.map visibleBothDirections |> Array.transpose

    let visibilityMap =
      Array.map2 (Array.map2 (||)) visibleHorizontal visibleVertical

    visibilityMap |> Array.collect id |> Array.filter id |> Array.length

module Part2 =
  let visibleTrees startingValue (row: int[]) =
    row
    |> Array.mapFold
      (fun blocked x -> not blocked, blocked || (x >= startingValue))
      false
    |> fst

  let scoresOneRow (row: int[]) =
    row
    |> Array.mapi (fun i x ->
      let toTheLeft =
        row[0 .. i - 1]
        |> Array.rev
        |> visibleTrees x
        |> Array.filter id
        |> Array.length

      let toTheRight =
        row[i + 1 ..] |> visibleTrees x |> Array.filter id |> Array.length

      toTheLeft * toTheRight
    )

  let prettyPrintMatrix (m: int[][]) =
    m
    |> Array.map (fun row -> row |> Array.map string |> String.concat " ")
    |> String.concat "\n"
    |> printfn "%s"

  let solve (input: string) =
    let m = getMatrix input
    let scoresHorizontal = m |> Array.map scoresOneRow

    let scoresVertical =
      m |> Array.transpose |> Array.map scoresOneRow |> Array.transpose

    let scoresMap = Array.map2 (Array.map2 (*)) scoresHorizontal scoresVertical

    scoresMap |> Array.collect id |> Array.max

let s1 = Part1.solve INPUT
let s2 = Part2.solve INPUT

(*
I really need to stop attempting to fix solution by trying random things. It sucks my energy out.
I should have just take it slow, not panic and solve it piece by piece, checking for all edge cases.
*)
