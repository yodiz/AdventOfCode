#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let test1  = load folder "test1.txt"
let test2  = load folder "test2.txt"
    
let p1 (a:string array) = 
    let x = 
        a
        |> Array.map (fun x -> Text.split2 "   " x)
    let a = x |> Array.map (fst >> Parse.int32) |> Array.sort
    let b = x |> Array.map (snd >> Parse.int32) |> Array.sort
    Array.map2 (fun a b -> abs (a - b)) a b
    |> Array.sum
    
let p2 (a:string array) = 
    let x = a |> Array.map (fun x -> Text.split2 "   " x)
    let a = x |> Array.map (fst >> Parse.int32) 
    let b = x |> Array.map (snd >> Parse.int32) |> Array.groupBy id |> Array.map (fun (x,xx) -> x, xx.Length) |> Map.ofArray
    a
    |> Array.map (fun x -> x * (b |> Map.tryFind x |> Option.defaultValue 0))
    |> Array.sum

Test.equal "Test1" 11 (p1 test1)
Test.equal "Test1" 31 (p2 test1)

let part1 = p1 input
let part2 = p2 input

printfn "Part1: %i" part1
printfn "Part2: %i" part2
