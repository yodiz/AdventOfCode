#if INTERACTIVE
#load "../Common.fsx"
#else
module Day6
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let input  = loadAll folder "input.txt"


let findMarker (arr:char array) (markLen:int) = 
    arr
    |> Array.mapi 
        (fun i x -> 
            if i >= markLen - 1 then
                let l = 
                    [0..markLen - 1]
                    |> List.map (fun x -> arr.[i - x])
                    |> Set.ofList
                    |> Set.count
                if l = markLen then 
                    true
                else false
            else
                false 
        )
    |> Array.findIndex (fun x -> x)
    |> ((+)1)


let part1 = findMarker (input.ToCharArray()) 4
let part2 = findMarker (input.ToCharArray()) 14

//
printfn "%i" part1
printfn "%i" part2

