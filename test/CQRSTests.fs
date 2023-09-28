module CQRS.Tests

open Expecto
open Swensen.Unquote
open Common

[<Tests>]
let tests =
    testLabel "Unit" <| testList "CQRS" [
        testCase "CQRS diffing should return the correct end state and a diff between known history and total history" <| fun _ ->
            let initial = 7
            let log = [(+) 1, 8; (*) 2, 16; flip (-) 3, 13; flip (/) 4, 3; (+) 15, 18] // include the expected result as a label of sorts just to make verification easier, since we cannot compare functions
            let update msg (model: int) : int =
                match msg with
                | f, expected ->
                    test <@ f model = expected @>
                    f model
            test <@ cqrsDiff update (fun _ _ (_, expected) -> Some expected) (initial + 1, log[0..0]) (log |> List.rev) = (18, [16; 13; 3; 18]) @>
    ]
