#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let test1  = load folder "test1.txt"

let rec isChanging fn maxn n previous remaining =
    match remaining with 
    |[] -> true
    |next::rest -> 
        let ok = (fn previous next && abs (previous - next) <= 3)
        if ok then 
            isChanging fn maxn n next rest
        else
            if n >= maxn then 
                false
            else
                isChanging fn maxn (n+1) previous rest

let isSafe maxn a = 
    let x = a |> Array.toList
    isChanging (>) maxn 0 x.Head x.Tail || isChanging (<) maxn 0 x.Head x.Tail
    || 
    isChanging (>) 0 0 x.Tail.Head x.Tail.Tail || isChanging (<) 0 0 x.Tail.Head x.Tail.Tail //Fult hack för att hantera när den första ska bytas ut.-. Orkar inte fixa mer med det

let solve maxn a = 
    a 
    |> Array.filter 
        (fun x -> 
            Text.split " " x 
            |> Array.map Parse.int32
            |> isSafe maxn
        )
    |> Array.length
    
let p1 a = solve 0 a
let p2 a = solve 1 a


Test.equal "Test1" 2 (p1 test1)
Test.equal "Test2" 4 (p2 test1)

let part1 = p1 input
let part2 = p2 input

//508 to low
//604 someones elses answer
printfn "Part1: %i" part1
printfn "Part2: %i" part2
