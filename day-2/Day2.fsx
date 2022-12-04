open System.IO
let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

type RPS =
  | Rock = 1
  | Paper = 2
  | Scissors = 3

// a - b
// -2, 1 = A wins
// -1, 2 = B wins
// 0 = Draw

type Outcome =
  | Win = 6
  | Draw = 3
  | Lose = 0

let parseRPS (c: string) =
  match c with
  | "A"
  | "X" -> RPS.Rock
  | "B"
  | "Y" -> RPS.Paper
  | "C"
  | "Z" -> RPS.Scissors
  | i -> failwith $"Invalid RPS: {i}"

let parseOutcome (c: string) =
  match c with
  | "X" -> Outcome.Lose
  | "Y" -> Outcome.Draw
  | "Z" -> Outcome.Win

let rpsToOutcome (my: RPS) (op: RPS) =
  match (int my) - (int op) with
  | -2
  | 1 -> Outcome.Win
  | -1
  | 2 -> Outcome.Lose
  | 0 -> Outcome.Draw
  | i -> failwith $"Invalid game result: {i}"

let findMyRPS (outcome: Outcome) (op: RPS) =
  match outcome with
  | Outcome.Win ->
    match op with
    | RPS.Rock -> RPS.Paper
    | RPS.Paper -> RPS.Scissors
    | RPS.Scissors -> RPS.Rock
  | Outcome.Draw -> op
  | Outcome.Lose ->
    match op with
    | RPS.Rock -> RPS.Scissors
    | RPS.Paper -> RPS.Rock
    | RPS.Scissors -> RPS.Paper

let getRoundScoreP1 (i: string) =
  let tok = i.Split(' ')
  let op = parseRPS (tok[0])
  let my = parseRPS (tok[1])
  (int my) + int (rpsToOutcome my op)

let getRoundScoreP2 (i: string) =
  let tok = i.Split(' ')
  let op = parseRPS (tok[0])
  let outcome = parseOutcome (tok[1])
  let my = findMyRPS outcome op
  (int my) + int outcome

let part1 (input: string) =
  input.Split("\n")
  |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
  |> Array.map getRoundScoreP1
  |> Array.sum


let part2 (input: string) =
  input.Split("\n")
  |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
  |> Array.map getRoundScoreP2
  |> Array.sum

part1 INPUT
part2 INPUT
