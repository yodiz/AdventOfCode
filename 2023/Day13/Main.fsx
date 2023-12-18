#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = loadAll folder "input.txt"
let inptest1  = loadAll folder "test1.txt"
let inptest2  = loadAll folder "test2.txt"

let rec isVertReflection y x1 x2 (map:Pos.Map) = 
    if x1 < 0 || x2 >= map.Width then true
    else
        let c1 = map.Map |> Map.find (Pos.create x1 y)
        let c2 = map.Map |> Map.find (Pos.create x2 y)
        if c1 = c2 then  isVertReflection y (x1-1) (x2+1) map
        else false

let rec isHoriReflection x y1 y2 (map:Pos.Map) = 
    if y1 < 0 || y2 >= map.Height then true
    else
        let c1 = map.Map |> Map.find (Pos.create x y1)
        let c2 = map.Map |> Map.find (Pos.create x y2)
        if c1 = c2 then  isHoriReflection x (y1-1) (y2+1) map
        else false

let findRefVert (map:Pos.Map) = //**..**
    [0..map.Height-1]
    |> List.map 
        (fun y -> 
            [0..map.Width-1] 
            |> List.pairwise
            |> List.filter (fun (x1,x2) -> isVertReflection y x1 x2 map)
            |> Set.ofList
        )
    |> List.reduce Set.intersect 
    |> (fun x -> 
            let l = Set.count x; 
            if l = 0 then None elif l = 1 then Some (Set.toList x |> List.head) else failwithf "WTF?")

let findRefHorz (map:Pos.Map) = //**..**
    [0..map.Width-1]
    |> List.map 
        (fun x -> 
            [0..map.Height-1] 
            |> List.pairwise
            |> List.filter (fun (y1,y2) -> isHoriReflection x y1 y2 map)
            |> Set.ofList
        )
    |> List.reduce Set.intersect 
    |> (fun x -> 
            let l = Set.count x; 
            if l = 0 then None elif l = 1 then Some (Set.toList x |> List.head) else failwithf "WTF?")



let solve1 input = 
    let maps = Text.split "\r\n\r\n" input
    let maps = 
        maps 
        |> Array.map (fun m -> Pos.Map.parse ' ' (Text.split "\r\n" m ))
        |> Array.map 
            (fun map -> 
                match findRefVert map with
                |Some (_,x) -> x
                |None -> match findRefHorz map with |Some (_,x) -> x*100 |None -> failwithf "WTF2?"
            )
        |> Array.sum
    maps

let solve2 input =  
    0

let test1 = solve1 inptest1 
// let test2 = solve2 inptest1 
let part1 = solve1 input
// let part2 = solve2 input 
