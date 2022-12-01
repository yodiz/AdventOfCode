#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day5
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"


type C = {X: int; Y:int}

let expandLine allowDiagonal (a:C) (b:C)  = 
    if a.X = b.X || a.Y = b.Y then
        [|
            for x = min a.X b.X to max a.X b.X  do
                for y = min a.Y b.Y to max a.Y b.Y  do
                    yield { X = x; Y = y }
        |]
    else
        if allowDiagonal then        
            [|
                for i = 0 to (max a.X b.X - min a.X b.X) do
                    let xt = if b.X > a.X then 1 else -1
                    let yt = if b.Y > a.Y then 1 else -1
                    yield { X = a.X + (i * xt); Y =  a.Y + (i * yt)} 
            |]
        else
            [||]


let parseLine (str:string) = 
    let parseco (str:string) = let (a,b) = Text.split2 "," str in {X = Int32.Parse a; Y = Int32.Parse b }
    let (from,toCo) = Text.split2 " -> " str  
    parseco from, parseco toCo
  
let input = load folder "input.txt" |> Array.map parseLine
    

let countCrosses input expandFn = 
    input
    |> Array.collect (fun (a,b) -> expandFn a b)
    |> Array.fold 
        (fun m x -> 
            let c = 
                match m |> Map.tryFind x with 
                |Some s -> s 
                |None -> 0
            m |> Map.add x (c+1)
        ) Map.empty
    |> Map.toList
    |> List.filter (fun (_,x) -> x > 1)
    |> List.length    

let part1 = countCrosses input (expandLine false) //7674
let part2 = countCrosses input (expandLine true)  //20898


"   ".Split(' ')