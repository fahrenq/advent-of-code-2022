open System
open System.IO
open System.Text.RegularExpressions

let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

let TEST_INPUT =
  """2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"""

let parseInput (input: string) =
  input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
  |> Array.map (fun line ->
    let t = line.Split(",")
    (int t[0], int t[1], int t[2])
  )

// 1. count all cubes sides as if they're not touching
let ALL_CUBES = parseInput INPUT
// let ALL_CUBES = parseInput INPUT

let plusMinus1 a b = abs (a - b) = 1

let findCubesAround allCubes (x, y, z) =
  allCubes
  |> Array.filter (fun (x', y', z') ->
    // x AND y is the same, Z is ± 1
    // Z is the same AND x OR y is ± 1
    (x = x' && y = y' && plusMinus1 z z')
    || (z = z' && ((y = y' && plusMinus1 x x') || (x = x' && plusMinus1 y y')))
  )

let findVisibleSides allCubes =
  let totalCubes = allCubes |> Array.length
  let totalCubeSides = totalCubes * 6
  // 2. count all cubes touching each other
  let blockedSidesPerCube =
    allCubes |> Array.map (findCubesAround allCubes >> Array.length)

  let totalBlockedSides = (blockedSidesPerCube |> Array.sum)
  // 3. subtract blocked sides from total sides
  let totalVisibleSides = (totalCubeSides - totalBlockedSides)
  totalVisibleSides

// PT1
findVisibleSides ALL_CUBES

// 4. add air cubes
// BELOW ONLY COUNTS A SINGLE EMPTY CUBES, WRONG
let trappedSpaceToTheRight allCubes (x, y, z) =
  // we can check only one side of the cube
  // let's assume we check x+1 location
  // we need to find 3 cubes with the following conditions
  // ==z, ==y, x+2
  // OR
  // z±1, ==y, x+1
  // THE SPACE CAN BE EITHER CUBE OR AIR
  // CHECK IF IT'S AIR LATER
  let cubesThatTrap =
    allCubes
    |> Array.filter (fun (x', y', z') ->
      (z = z' && y = y' && (x' - 2 = x))
      || (plusMinus1 z z' && y = y' && (x' - 1 = x))
    )

  if cubesThatTrap.Length = 3 then Some(x + 1, y, z) else None

let trappedSpacesToTheRight allCubes (x, y, z) =
  // opposite cube = == z, ==y, FIRST > x+1 --- (count 1)
  // z trapping cubes = z±1, ==y, x' > x but x' < x'' (count x''-x)

  let oppositeCube =
    allCubes
    |> Array.sortBy (fun (x, _, _) -> x)
    |> Array.tryFind (fun (x', y', z') -> z' = z && y' = y && x' - 1 > x)

  let emptySpacesCount =
    match oppositeCube with
    | Some (x', _, _) -> x' - x - 1
    | None -> 0


  if emptySpacesCount > 0 then
    let opX, _, _ = oppositeCube.Value

    // now check z trapping cubes
    let zTrappingCubes =
      allCubes
      |> Array.filter (fun (x', y', z') ->
        plusMinus1 z z' && y = y' && x' > x && (x' > x && x' < opX)
      )

    let zTrappingCubesCount = zTrappingCubes.Length

    if zTrappingCubesCount = emptySpacesCount * 2 then
      Array.init emptySpacesCount (fun i -> (x + i + 1, y, z))
    else
      [||]
  else
    [||]

let airCubes =
  ALL_CUBES
  |> Array.collect (trappedSpacesToTheRight ALL_CUBES)
  |> Array.filter (fun c -> Array.contains c ALL_CUBES |> not)

// let airCubesVisibleSides = findVisibleSides airCubes
let airCubesVisibleSides = airCubes.Length * 6

// PT 2 ANSWER
let totalVisibleSidesMinusTrappedAir =
  findVisibleSides ALL_CUBES - airCubesVisibleSides

// 3144 - wrong, too high
// 2968 - wrong, too high
// 2938

//
let X_START, Y_START, Z_START =
  let x = ALL_CUBES |> Array.map (fun (x, _, _) -> x) |> Array.min
  let y = ALL_CUBES |> Array.map (fun (_, y, _) -> y) |> Array.min
  let z = ALL_CUBES |> Array.map (fun (_, _, z) -> z) |> Array.min
  (x - 1, y - 1, z - 1)

let X_END, Y_END, Z_END =
  let x = ALL_CUBES |> Array.map (fun (x, _, _) -> x) |> Array.max
  let y = ALL_CUBES |> Array.map (fun (_, y, _) -> y) |> Array.max
  let z = ALL_CUBES |> Array.map (fun (_, _, z) -> z) |> Array.max
  (x + 1, y + 1, z + 1)

type Coordinate = int * int * int


let gn (x, y, z) : Coordinate list =
  [
    x + 1, y, z
    x - 1, y, z
    x, y + 1, z
    x, y - 1, z
    x, y, z + 1
    x, y, z - 1
  ]
  |> List.filter (fun (x, y, z) ->
    x >= X_START
    && x <= X_END
    && y >= Y_START
    && y <= Y_END
    && z >= Z_START
    && z <= Z_END
  )

X_END

gn (2, 2, 2)

let bfs (getNeighbours: Coordinate -> Coordinate list) =
  let rec bfs'
    (queue: Coordinate list)
    (visited: Set<Coordinate>)
    (sides: Map<Coordinate, int>)
    =
    match queue with
    | [] -> sides.Values |> Seq.sum
    | current :: rest ->
      let neighbours = getNeighbours current
      let newVisited = visited.Add current
      let cubesAround = findCubesAround ALL_CUBES current
      let newSides = sides.Add(current, cubesAround.Length)

      let emptyNeighbours =
        neighbours
        |> List.filter (fun n -> not (cubesAround |> Array.contains n))

      let newQueue =
        (rest @ emptyNeighbours) |> List.filter (visited.Contains >> not)

      bfs' (newQueue) newVisited newSides

  bfs' [ X_START, Y_START, Z_START ] Set.empty Map.empty

bfs gn

(*
That was first time I had to use BFS, and I had to look up the algorithm.
I definitely could make it a lot prettier.
People assessing my coding skills: Look someplace else.
*)
