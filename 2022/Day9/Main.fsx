#if INTERACTIVE
#load "../Common.fsx"
#else
module Day9
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let parseLine (str:string) = 
    let d, s = AoC.Text.split2 " " str
    d[0],Parse.int32 s

let input  = load folder "input.txt" |> Array.map parseLine
type Point = { x:int; y:int }
let point x y = { x = x; y = y }
type Board = { Knots : Point list; Visited : Set<Point> }

let maxOne n = if n < 0 then -1 elif n = 0 then 0 else 1


let calc knots = 
    input
    |> Array.collect 
        (fun (d,s) -> 
            [|0..s-1|]
            |> Array.map (fun _ -> d)
        )
    |> Array.fold 
        (fun (b:Board) d -> 
            let newKnots = 
                b.Knots
                |> List.fold 
                    (fun nn x -> 
                        match nn with 
                        |[] -> 
                            match d with
                            |'R' -> point (x.x + 1) x.y
                            |'L' -> point (x.x - 1) x.y
                            |'U' -> point x.x (x.y - 1)
                            |'D' -> point x.x (x.y + 1)
                            |_ -> failwithf "Not supp"
                            |> (fun n -> [n])
                        |n::tail -> 
                            let t = 
                                let dx = n.x - x.x 
                                let dy = n.y - x.y
                                if abs dx < 2 && abs dy < 2 then
                                    //Touching - no move
                                    x
                                elif abs dx = 2 && abs dy = 0 then
                                    point (x.x + (maxOne dx)) x.y
                                elif abs dy = 2 && abs dx = 0 then
                                    point x.x (x.y + (maxOne dy))
                                elif abs dx <> 0 && abs dy <> 0 then
                                    //Move diagonally
                                   point (x.x + (maxOne dx)) (x.y + (maxOne dy))
                                else failwithf "BAJS"

                            t::n::tail
                    )
                    []

            { Knots = newKnots |> List.rev; Visited = b.Visited |> Set.add newKnots.Head }
        )
        { Knots = [0..knots-1] |> List.map (fun _ -> point 0 0); Visited = Set.empty }
     |> fun x -> x.Visited |> Set.count

     
let part1 = calc 2
let part2 = calc 10

printfn "%i" part1
printfn "%i" part2

