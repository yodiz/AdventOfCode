#if INTERACTIVE
#load "../Common.fsx"
#else
module Day8
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let parseLine (str:string) = 
    str.ToCharArray() |> Array.map (fun c -> int c)

let input  = load folder "input.txt" |> Array.map parseLine

let rec visit (map:int array array) (fromX, fromY) (stepX, stepY) (maxX, maxY) h = 
    let x = fromX + stepX
    let y = fromY + stepY

    if x < 0 || y < 0 || x >= maxX || y >= maxY then 
        true
    else    
        let ch = map[y][x]
        if ch < h then 
            visit map (x,y) (stepX, stepY) (maxX, maxY)  h
        else false

let rec scan s (map:int array array) (fromX, fromY) (stepX, stepY) (maxX, maxY) h = 
    let x = fromX + stepX
    let y = fromY + stepY

    if x < 0 || y < 0 || x >= maxX || y >= maxY then 
        s
    else    
        let ch = map[y][x]
        if ch < h then 
            scan (s+1) map (x,y) (stepX, stepY) (maxX, maxY)  h
        else s + 1
    
let isVisible (map:int array array) (x:int,y:int) = 
    let h = map[y][x]
    let visRight = visit map (x,y) (1,0) (map[0].Length,map.Length) h
    let visLeft = visit map (x,y) (-1,0) (map[0].Length,map.Length) h
    let visBottom = visit map (x,y) (0,1) (map[0].Length,map.Length) h
    let visTop = visit map (x,y) (0,-1) (map[0].Length,map.Length) h
    
    visRight || visLeft ||visBottom || visTop

let score (map:int array array) (x:int,y:int) = 
    let h = map[y][x]
    let visRight = scan 0 map (x,y) (1,0) (map[0].Length,map.Length) h
    let visLeft = scan 0  map (x,y) (-1,0) (map[0].Length,map.Length) h
    let visBottom = scan 0 map (x,y) (0,1) (map[0].Length,map.Length) h
    let visTop = scan 0 map (x,y) (0,-1) (map[0].Length,map.Length) h

    visRight * visLeft * visBottom * visTop

let part1 = 
    input
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x _h -> isVisible input (x,y)) |> Array.sumBy (fun x -> if x then 1 else 0))
    |> Array.sum

let part2 = 
    input
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x _h -> score input (x,y)))
    |> Array.collect id
    |> Array.max

score input (2,3)


//
printfn "%i" part1
printfn "%i" part2

