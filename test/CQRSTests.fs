module CQRS.Tests

open Expecto
open Swensen.Unquote

let cqrsDiff update (currentState: 'model, knownMsgs: 'msg list) (msgs': 'msg list): 'model * 'msg list =
    let newMessages = msgs' |> List.take (msgs'.Length - knownMsgs.Length)
    let rec loop model newMessagesRev = function
        | msg ->
            ()
    notImpl()
let update msg model = function
    | f, expected ->
        test <@ f model = expected @>
        model

[<Tests>]
let tests =
    testLabel "POC TDD" <| testList "CQRS" [
        testCase "Design CQRS diffing" <| fun _ ->
            let initial = 7
            let msgs = [(+) 1, 8; (*) 2, 16; flip (-) 3, 13; flip (/) 4, 3; (+) 15, 18] // include the expected result as a label of sorts just to make verification easier, since we cannot compare functions

            test <@ cqrsDiff update (initial + 1, List.rev msgs) [(+) 1, 8] |> Tuple2.mapsnd (List.map snd) = (18, [16; 13; 3; 18]) @>
    ]
