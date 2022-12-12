open System
open System.IO
open System.Text.RegularExpressions

let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

type Tree<'LeafData, 'INodeData> =
  | LeafNode of 'LeafData
  | InternalNode of 'INodeData * Tree<'LeafData, 'INodeData> seq

type FileInfo = { name: string; fileSize: int }
type DirectoryInfo = { pwd: string }
type FileSystemItem = Tree<FileInfo, DirectoryInfo>

let mapLast f (xs: _[]) =
  let length = xs |> Array.length
  xs |> Array.mapi (fun idx i -> if idx = length - 1 then f i else i)

module Tree =
  let rec cata fLeaf fNode (tree: Tree<'LeafData, 'INodeData>) : 'r =
    let recurse = cata fLeaf fNode

    match tree with
    | LeafNode leafInfo -> fLeaf leafInfo
    | InternalNode (nodeInfo, subtrees) ->
      fNode nodeInfo (subtrees |> Seq.map recurse)

  let rec extend predicate newSubtrees tree =
    match tree with
    | LeafNode leafInfo -> LeafNode leafInfo
    | InternalNode (nodeInfo, subtrees) ->
      if predicate nodeInfo then
        if subtrees |> Seq.length <> 0 then
          failwithf "NON EMPTY EXTEND"

        InternalNode(nodeInfo, newSubtrees)
      else
        InternalNode(
          nodeInfo,
          subtrees |> Seq.map (extend predicate newSubtrees)
        )


let rec prettyPrintFs indent (tree: FileSystemItem) =
  match tree with
  | LeafNode i -> printfn $"{indent}- {i.name} ({i.fileSize})"
  | InternalNode (i, subtree) ->
    printfn $"{indent}- {i.pwd}"
    subtree |> Seq.iter (prettyPrintFs (indent + "  "))

let parseLsResponseToSubtrees pwd (lsResponse: string[]) =
  let subtrees =
    lsResponse
    |> Seq.map (fun line ->
      let tokens = line.Split " "

      match tokens[0], tokens[1] with
      | "dir", name -> InternalNode({ pwd = $"{pwd}{name}/" }, Seq.empty)
      | size, name -> LeafNode({ name = name; fileSize = int size })
    )

  subtrees

let inputToCommandsWithOutput (input: string) =
  input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
  |> Array.fold
    (fun (commandsWithOutput: (string * string[])[]) line ->
      match line[0] with
      | '$' ->
        Array.append commandsWithOutput [| line.Replace("$ ", ""), [||] |]
      | _ ->
        commandsWithOutput
        |> mapLast (fun (command, output) ->
          (command, (Array.append output [| line |]))
        )
    )
    [||]

let parentPwd (pwd: string) =
  if pwd = "/" then "/" else Regex.Replace(pwd, "/[^/]+/$", "/")

let getFileSystem input =
  let commandsWithOutput = inputToCommandsWithOutput input
  let start = InternalNode({ pwd = "/" }, Seq.empty)

  commandsWithOutput
  |> Array.fold
    (fun (pwd: string, currentFS: FileSystemItem) (command, output) ->
      match command with
      | "cd .." -> parentPwd pwd, currentFS
      | "cd /" -> "/", currentFS
      | c when c.StartsWith("cd") ->
        let t = c.Split " "
        let path = t[1] + "/"
        pwd + path, currentFS
      | "ls" ->
        let newSubtree = parseLsResponseToSubtrees pwd output

        let newCurrentFs =
          Tree.extend
            (fun (i: DirectoryInfo) -> i.pwd = pwd)
            newSubtree
            currentFS

        pwd, newCurrentFs
      | unknownCommand ->
        failwithf $"unknown command: %s{unknownCommand}"
        pwd, currentFS
    )
    ("/", start)


let TEST_INPUT =
  """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""

let pwd, fs = getFileSystem INPUT

let directorySize fs =
  Tree.cata
    (fun (i: FileInfo) -> i.fileSize)
    (fun (_: DirectoryInfo) (subtreeSizes: int seq) -> subtreeSizes |> Seq.sum)
    fs

let rec dirListWithSize (tree: FileSystemItem) =
  match tree with
  | LeafNode _ -> Seq.empty
  | InternalNode (dirInfo, sub) as self ->
    Seq.append
      [| dirInfo, directorySize self |]
      (Seq.collect dirListWithSize sub)

// part 1
dirListWithSize fs
|> Seq.filter (fun (_, size) -> size < 100000)
|> Seq.sumBy (fun (_, size) -> size)

// part 2
let current = directorySize fs

let TOTAL = 70000000
let REQUIRED = 30000000
let diff = TOTAL - current
let toFreeUp = REQUIRED - diff

dirListWithSize fs
|> Seq.filter (snd >> (<) toFreeUp)
|> Seq.sortBy snd
|> Seq.head


(*
Holy shit I am bad with trees. I guess the lack of the formal computer-science education is showing.
Most of the trees-related code here I blandly stole from F# For Fun And Profit and Copilot helped me doing that.
Reconstructing file system from the input and extend function I wrote myself. Nevertheless I'm proud.
*)
