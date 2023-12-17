#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"
let inptest2  = load folder "test2.txt"

open Microsoft.FSharp.Core.Operators.Checked

let parseLine y (line:string) = 
    line.ToCharArray() 
    |> Array.mapi (fun x l -> match l with |'.' -> None |c -> Some ((Pos.create x y), c))
    |> Array.choose id
    

let parse input = 
    input |> Array.mapi (fun y l -> parseLine y l)
    |> Array.collect id
    |> Map.ofArray
    ,input[0].Length,input.Length

let rec tick width height direction stones = 
    let moved, stones = 
        stones 
        |> Map.fold 
            (fun (m,s) (pos:Pos) (thing:char) -> 
                match thing with 
                |'O' ->
                    //Check if it can move north - if it can do and save that something moved
                    let targetPos = Pos.add pos direction
                    let targetThing = s |> Map.tryFind targetPos |> Option.defaultValue '.'
                    if targetPos.y >= 0 && targetPos.x >= 0 && targetPos.y < height && targetPos.x < width && targetThing = '.' then
                        // printfn "Moving %A to %A" pos targetPos
                        true,(s |> Map.remove pos |> Map.add targetPos thing)
                    else 
                        m,s
                | _ -> m,s
            )
            (false,stones)
    if moved then
        tick width height direction stones
    else
        stones

let load height stones =
    stones
    |> Map.fold 
        (fun s pos thing -> 
            match thing with 
            |'O' -> s + (int64 (height - int pos.y))
            |_ -> s
        ) 
        0L

let solve1 input = 
    let stones,width,height = parse input
    let move = tick width height (Pos.create 0 -1) stones
    load height move

let solve2 input = 
    let stones,width,height = parse input
    let mutable st = stones

    let mutable pos = Map.empty

    //repetera tills vi hittar ett repeterade mönster
    //Om vi sparar ner stenarnas position, så 
    let cycles = 1000000000

    for i = 1 to cycles do 
        let print () = 
            // if i % i = 0 then 
                let load = load height st
                printfn "%i - Load: %i" i load    

        
        st <- tick width height (Pos.create 0 -1) st
        st <- tick width height (Pos.create -1 0) st
        st <- tick width height (Pos.create 0 1) st
        st <- tick width height (Pos.create 1 0) st

        match pos |> Map.tryFind st with 
        |Some (lastI:int) -> 
            let delta = (lastI - i)
            // printfn "Repeating %i -> %i (Delta: %i)" lastI i (lastI - int64 i)
            let remainingCycles = cycles - i
            if remainingCycles % delta = 0 then
                let load = load height st
                printfn "Found possible solution? %i @ %i" load i
                System.Threading.Thread.Sleep(5000)
            ()
        |None -> ()
        pos <- pos |> Map.add st i

        // print () 

    0

let test1 = solve1 inptest1 // 136L
// let test2 = solve2 inptest1 // 64


let part1 = solve1 input // 110677L
// let part2 = solve2 input
