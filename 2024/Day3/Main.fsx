#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = loadAll folder "input.txt"
let test1  = loadAll folder "test1.txt"
let test2  = loadAll folder "test2.txt"

open AoC.Expect

type ASL = |MUL of int*int |DO |DONT

let eMul = eString "mul(" >. eInt .> eString "," .>. eInt .> eString ")" |> map (MUL)
let eDont = eString "don't()" |> map (fun _ -> DONT)
let eDo = eString "do()" |> map (fun _ -> DO)

let eAst = eEither eMul eDont |> eEither eDo 

let p1 (a:string) = 
    [0..a.Length-1] 
    |> List.choose (fun index -> runOpt a index eMul)
    |> List.fold (fun s -> (function |MUL (a,b) -> s + a*b |_ -> failwithf "")) 0 

let p2 (a:string) = 
    [0..a.Length-1] 
    |> List.choose (fun index -> runOpt a index eAst)
    |> List.fold 
        (fun (e,s) x -> 
            printfn "%A" x
            match x with
            |MUL (a,b) -> (e,s+if e then (a*b) else 0)
            |DO -> (true,s)
            |DONT -> (false,s)
        ) 
        (true,0)
    |> snd


Test.equal "Test1" 161 (p1 test1)
Test.equal "Test2" 48 (p2 test2)

let part1 = p1 input
let part2 = p2 input

printfn "Part1: %i" part1
printfn "Part2: %i" part2
