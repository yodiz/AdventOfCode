#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = loadAll folder "input.txt"
let inptest1  = loadAll folder "test1.txt"
let inptest2  = loadAll folder "test2.txt"

// let reflection_row (block: string array) : int =
//     let mutable res = 0
//     for idx = 1 to block.Length - 1 do 
//         let bottom = block[idx..] |> Array.rev
//         let top = block[..idx-1]
//         let x = Seq.zip top bottom |> Seq.toArray
//         printfn "%A\r\n" x
//         let isReflect = x |> Seq.forall (fun (a,b) -> a = b)
//         if isReflect then 
//             printfn "%i" idx
//             res <- idx 
//         // printfn "%A" x
//     res
// [1;2;2;1;2;1][2..] |> List.rev
// [1;2;2;1;2;1][..2-1]


// let maps = Text.split "\r\n\r\n" inptest1 |> Array.map (fun x -> Text.split "\r\n" x)
// reflection_row (maps[0])
// reflection_row (maps[1])

let rec isLineReflection y x1 x2 (map:Pos.Map) s = 
    if x1 < 0 || x2 >= map.Width then s
    else
        let c1 = map.Map |> Map.find (Pos.create x1 y)
        let c2 = map.Map |> Map.find (Pos.create x2 y)
        if c1 = c2 then isLineReflection y (x1-1) (x2+1) map s
        else isLineReflection y (x1-1) (x2+1) map (s+1)



// #.##..#
// ..##..#


let findRefVert (map:Pos.Map) = //**..**
    [0..map.Height-1]
    |> List.map 
        (fun y -> 
            [0..map.Width-1] 
            |> List.pairwise
            |> List.filter (fun (x1,x2) -> isLineReflection y x1 x2 map 0 = 0)
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
                |Some (_,x) -> x*1
                |None -> match findRefVert (Pos.Map.rotate map) with |Some (_,x) -> x*100 |None -> failwithf "WTF2?"
            )
        |> Array.sum
    maps


let findRefVert2 (map:Pos.Map) = //**..**
    let r = 
        [0..map.Height-1]
        |> List.map 
            (fun y -> 
                //Kolla alla rader 
                let r = 
                    [0..map.Width-1] 
                    |> List.pairwise
                    |> List.map (fun (x1,x2) -> (x1,y), isLineReflection y x1 x2 map 0)
                r
                //Det index där summan är = 1
            )
    let q = 
        r
        |> List.collect id
        |> List.groupBy (fun ((x,y),n) -> x)
        |> List.filter (fun (x,things) -> things |> List.sumBy (fun ((x,y),n) -> n) = 1)
        |> function |[] -> None |a::[] -> Some a |_ -> failwithf ""
        |> Option.map fst
    q

//Vidrigt - men det fungerar så det får va. Tog mycket längre tid än det borde.
let solve2 input = 
    let maps = Text.split "\r\n\r\n" input
    let maps = 
        maps 
        |> Array.map (fun m -> Pos.Map.parse ' ' (Text.split "\r\n" m ))
        |> Array.map 
            (fun map -> 
                match findRefVert2 map with
                |Some (x) -> (x+1)*1
                |None -> match findRefVert2 (Pos.Map.rotate map) with |Some (x) -> (x+1)*100 |None -> failwithf "WTF2?"
            )
        |> Array.sum
    maps

    // findRefVert2 (maps[0]) 
    
let test1 = solve1 inptest1 
let test2 = solve2 inptest1 
let part1 = solve1 input
let part2 = solve2 input 
