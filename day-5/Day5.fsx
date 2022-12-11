open System
open System.IO

let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")


module Solution =

  type Stack = { Idx: char; Crates: char[] }
  type Move = { Count: int; FromIdx: char; ToIdx: char }

  let splitInput (input: string) =
    let inputParts = input.Split("\n\n")
    inputParts[0], inputParts[1]

  let stackPosAndIdFromIdsStrRaw (idsStr: string) =
    idsStr.ToCharArray()
    |> Array.indexed
    |> Array.filter (snd >> Char.IsWhiteSpace >> not)

  let crateToStackIdFromRow (stackIds: (int * char)[]) (row: string) =
    let crates = row.ToCharArray()

    stackIds
    |> Array.choose (fun (pos, stackId) ->
      crates
      |> Array.tryItem pos
      |> Option.filter ((=) ' ' >> not)
      |> Option.map (fun crate -> crate, stackId)
    )

  let fillStacks
    (allStackIds: char[])
    (crateToStackId: (char * char)[][])
    : Stack[] =

    allStackIds
    |> Array.map (fun stackId ->
      let crates =
        crateToStackId
        |> Array.choose (Array.tryFind (snd >> (=) stackId))
        |> Array.map fst

      { Idx = stackId; Crates = crates }
    )

  let parseInitialState (initialStacksRaw: string) =
    let initialStacksRows = initialStacksRaw.Split("\n")
    let stackIdsStr = initialStacksRows |> Array.last

    let crateRows =
      initialStacksRows |> Array.take (initialStacksRows.Length - 1)

    let stackIds = stackPosAndIdFromIdsStrRaw stackIdsStr

    let crateToStackId =
      crateRows |> Array.rev |> Array.map (crateToStackIdFromRow stackIds)

    let allStackIds = stackIds |> Array.map snd
    fillStacks allStackIds crateToStackId

  let parseMoveRow (moveRow: string) =
    let moveParts = moveRow.Split(" ")
    let count = int moveParts[1]
    let fromIdx = char moveParts[3]
    let toIdx = char moveParts[5]
    { Count = count; FromIdx = fromIdx; ToIdx = toIdx }

  let parseMoves (movesRaw: string) =
    movesRaw.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseMoveRow

  let pickUpCratesPt1 stacks move =
    (stacks |> Array.find (fun s -> s.Idx = move.FromIdx)).Crates
    |> Array.rev
    |> Array.take move.Count

  let pickUpCratesPt2 stacks move = pickUpCratesPt1 stacks move |> Array.rev

  let handleMove pickUpCrates (stacks: Stack[]) (move: Move) =
    let cratesToMove = pickUpCrates stacks move

    stacks
    |> Array.map (fun stack ->
      if stack.Idx = move.FromIdx then
        { stack with
            Crates =
              stack.Crates |> Array.take (stack.Crates.Length - move.Count)
        }
      elif stack.Idx = move.ToIdx then
        { stack with Crates = Array.append stack.Crates cratesToMove }
      else
        stack
    )


  let solvePt1 (input: string) =
    let initialStateRaw, movesRaw = splitInput input
    let initialState = parseInitialState initialStateRaw
    let moves = parseMoves movesRaw

    moves
    |> Array.fold (handleMove pickUpCratesPt1) initialState
    |> Array.map (fun s -> s.Crates |> Array.last)
    |> Array.map Char.ToString
    |> String.concat ("")

  let solvePt2 (input: string) =
    let initialStateRaw, movesRaw = splitInput input
    let initialState = parseInitialState initialStateRaw
    let moves = parseMoves movesRaw

    moves
    |> Array.fold (handleMove pickUpCratesPt2) initialState
    |> Array.map (fun s -> s.Crates |> Array.last)
    |> Array.map Char.ToString
    |> String.concat ("")

let solutionPt1 = Solution.solvePt1 INPUT
let solutionPt2 = Solution.solvePt2 INPUT
