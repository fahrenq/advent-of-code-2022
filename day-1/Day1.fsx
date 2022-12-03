open System.IO
let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

let part1 (input: string) =
  let groups = input.Split "\n\n"

  groups
  |> Array.map (fun g -> g.Split('\n') |> Array.map int |> Array.sum)
  |> Array.max

let part2 (input: string) =
  let groups = input.Split "\n\n"

  groups
  |> Array.map (fun g -> g.Split('\n') |> Array.map int |> Array.sum)
  |> Array.sortDescending
  |> Array.take 3
  |> Array.sum

printfn "Part 1: %d" (part1 INPUT)
printfn "Part 2: %d" (part2 INPUT)
