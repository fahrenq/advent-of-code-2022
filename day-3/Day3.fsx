open System.IO
let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

let intersect (s1: _[]) (s2: _[]) =
  s1 |> Array.filter (fun x -> Array.contains x s2) |> Array.distinct

let itemToPriority (item: char) =
  Array.concat [| [| 'a' .. 'z' |]; [| 'A' .. 'Z' |] |]
  |> Array.findIndex ((=) item)
  |> (+) 1

// part 1
INPUT.Split("\n")
|> Array.map (fun i -> i.ToCharArray())
|> Array.map (fun i -> i |> Array.splitAt (i.Length / 2))
|> Array.collect (fun (c1, c2) -> intersect c1 c2)
|> Array.map itemToPriority
|> Array.sum

// part 2
INPUT.Split("\n")
|> Array.map (fun i -> i.ToCharArray())
|> Array.chunkBySize 3
|> Array.map (fun i -> (intersect i[0] i[1]) |> intersect i[2])
|> Array.collect id
|> Array.map itemToPriority
|> Array.sum
