open System
open System.IO

let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

let TEST_INPUT =
  """30373
25512
65332
33549
35390"""

let merge (left: bool[]) (right: bool[]) =
  left |> Array.map2 (fun l r -> l || r) right

let getMatrix (input: string) =
  input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
  |> Array.map (fun r -> r.ToCharArray() |> Array.map (string >> Int32.Parse))

module Part1 =
  let visibleOneDirection row =
    row |> Array.mapFold (fun acc x -> x > acc, max x acc) Int32.MinValue |> fst

  let visibleBothDirections row =
    let d1 = visibleOneDirection row
    let d2 = visibleOneDirection (Array.rev row) |> Array.rev
    merge d1 d2


  let solve (input: string) =
    let m = getMatrix input
    let visibleHorizontal = m |> Array.map visibleBothDirections

    let visibleVertical =
      m |> Array.transpose |> Array.map visibleBothDirections |> Array.transpose

    let visibilityMap = Array.map2 merge visibleHorizontal visibleVertical

    visibilityMap |> Array.collect id |> Array.filter id |> Array.length

module Part2 =
  let solve (input: string) = -1
