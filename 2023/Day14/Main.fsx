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
    ,input.Length

let rec tick stones = 
    let moved, stones = 
        stones 
        |> Map.fold 
            (fun (m,s) (pos:Pos) (thing:char) -> 
                match thing with 
                |'O' ->
                    //Check if it can move north - if it can do and save that something moved
                    let targetPos = (Pos.add pos (Pos.create 0 -1))
                    let northThing = s |> Map.tryFind targetPos |> Option.defaultValue '.'
                    if pos.y > 0 && northThing = '.' then
                        // printfn "Moving %A to %A" pos targetPos
                        true,(s |> Map.remove pos |> Map.add targetPos thing)
                    else 
                        m,s
                | _ -> m,s
            )
            (false,stones)
    if moved then
        tick stones
    else
        stones

let solve1 input = 
    let stones,height = parse input
    let move = tick stones
    let load = 
        move 
            |> Map.fold 
                (fun s pos thing -> 
                    match thing with 
                    |'O' ->
                        // printfn "%A - %A" pos thing
                        let stonePos = height - int pos.y  
                        
                        s + (int64 stonePos)
                    |_ -> s
                ) 
                0L

    load

let test1 = solve1 inptest1

let part1 = solve1 input
// let part2 = solve 999999 input
