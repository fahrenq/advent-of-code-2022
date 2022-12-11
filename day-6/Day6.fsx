open System.IO
let INPUT = File.ReadAllText(__SOURCE_DIRECTORY__ + "/input.txt")

type ReceiverMode =
  | WaitingForPacketStarted of count: int * window: char[]
  | PacketStarted

let receiver windowSize =
  new MailboxProcessor<char>(fun inbox ->
    let rec loop mode =
      async {
        let! c = inbox.Receive()

        match mode with
        | PacketStarted -> return! loop PacketStarted
        | WaitingForPacketStarted (count, window) ->
          let nextWindow' = Array.append window [| c |]

          let nextWindow =
            if nextWindow'.Length > windowSize then
              Array.tail nextWindow'
            else
              nextWindow'

          let nextCount = count + 1

          let isNextWindowUnique =
            (nextWindow |> Array.distinct |> Array.length) = nextWindow.Length

          let isNextWindowFull = nextWindow.Length = windowSize

          if isNextWindowFull && isNextWindowUnique then
            printfn $"Found packet start: %i{nextCount}"
            return! loop PacketStarted
          else
            return! loop (WaitingForPacketStarted(nextCount, nextWindow))
      }

    loop (WaitingForPacketStarted(0, [||]))
  )

let receiver' = receiver 14
receiver'.Start()

let stream = INPUT.ToCharArray()
// let stream = "bvwbjplbgvbhsrlpgdmjqwftvncz".ToCharArray()
stream |> Array.iter (receiver'.Post)
