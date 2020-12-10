#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

let part1 =
    load "Day10/input.txt" |> Array.map Parse.int32 |> Array.sort
    |> Array.fold 
        (fun (p,m) j -> j, (Map.changeOrDefault (j - p) ((+)1) 1 m))
        (0,Map.empty)
    |> snd
    |> (fun x -> Map.find 1 x * ((Map.find 3 x) + 1))

let ways inp = 
    let inp = inp |> Array.map Parse.int32 |> Set.ofArray 
    let mutable mem = Map.empty
    let m = inp |> Set.maxElement
    
    let rec ways j = 
        if j = m then 1L
        else
            match mem |> Map.tryFind j with
            |Some x -> x
            |None -> 
                let r = [1..3] |> List.sumBy (fun d -> if inp |> Set.contains (j + d) then ways (j + d) else 0L)
                mem <- mem |> Map.add j r
                r
    ways 0

Test.equal "Test1" 8L (ways (load "Day10/test1.txt"))
Test.equal "Test2" 19208L (ways (load "Day10/test2.txt"))

let part2 = ways(load "Day10/input.txt")
