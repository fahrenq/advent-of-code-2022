open System.IO
let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

let splitInTwo (separator: char) mapFn (s: string) =
  let parts = s.Split(separator)
  mapFn parts.[0], mapFn parts.[1]

let fullOverlap (aStart, aEnd) (bStart, bEnd) =
  let c1 = aStart <= bStart && aEnd >= bEnd
  let c2 = bStart <= aStart && bEnd >= aEnd
  c1 || c2

let partialOverlap (aStart, aEnd) (bStart, bEnd) =
  let c1 = aStart <= bStart && aEnd >= bStart
  let c2 = bStart <= aStart && bEnd >= aStart
  c1 || c2


// part 1
INPUT.Split "\n"
|> Array.map (splitInTwo ',' id)
|> Array.map (fun (e1, e2) -> splitInTwo '-' int e1, splitInTwo '-' int e2)
|> Array.filter (fun (r1, r2) -> fullOverlap r1 r2)
|> Array.length

// part 2
INPUT.Split "\n"
|> Array.map (splitInTwo ',' id)
|> Array.map (fun (e1, e2) -> splitInTwo '-' int e1, splitInTwo '-' int e2)
|> Array.filter (fun (r1, r2) -> partialOverlap r1 r2)
|> Array.length
