module CommonTest
open FSharpPlus
open Expecto
open Library
open Common

let mapTests =
    test "map test" {
        let m1 = [ (1, "a"); (2, "b"); (3, "c") ] |> Map.ofList
        let m2 = [ (2, "b"); (3, "C"); (4, "D") ] |> Map.ofList

        let diff = Map.difference m1 m2
        // logger.I $"%A{diff}"
        Expect.equal
            diff
            (Map.ofList [ (1, "a") ])
            "difference"

        let inter = Map.intersect m1 m2
        Expect.equal
            inter
            (Map.ofList [ (2, "b"); (3, "c") ])
            "intersect"

        let interWith =
            Map.intersectWith (fun _ v2 -> v2) m1 m2
        Expect.equal
            interWith
            (Map.ofList [ (2, "b"); (3, "C") ])
            "intersectWith"

        let compare = Map.compare m1 m2

        let add =
            compare
            |> Map.chooseValues (fun v ->
                match v with
                | Some v1, None -> Some v1
                | _ -> None)
        Expect.equal
            add
            (Map.ofList [ (1, "a") ])
            "add"

        let delete =
            compare
            |> Map.chooseValues (fun v ->
                match v with
                | None, Some v2 -> Some v2
                | _ -> None)
        Expect.equal
            delete
            (Map.ofList [ (4, "D") ])
            "delete"

        let update =
            compare
            |> Map.chooseValues (fun v ->
                match v with
                | Some v1, Some v2 ->
                    Option.ofPair (v1 <> v2, (v1, v2))
                | _ -> None)
        Expect.equal
            update
            (Map.ofList [ (3, ("c", "C")) ])
            "update"

        let forSeq (s: int seq) =
            let mutable result = 0
            for i in s do
                result <- result + i
            result

        let fa = forSeq [| 1; 2; 3 |]
        Expect.equal fa 6 "arrary"

        let fl = forSeq [ 1; 2; 3 ]
        Expect.equal fl 6 "list"
    }
