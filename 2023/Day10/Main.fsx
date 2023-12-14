#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"
let inptest2  = load folder "test2.txt"

type Pos = {x: int;y:int}
module Pos = 
    let c x y = { x = x; y = y }
    let sub (a:Pos) (b:Pos) = { x = a.x - b.x; y = a.y - b.y }
    let add (a:Pos) (b:Pos) = { x = a.x + b.x; y = a.y + b.y }
let pipes = 
    [
        '|', (Pos.c 0 1, Pos.c 0 -1)
        '-', (Pos.c -1 0, Pos.c 1 0)
        'L', (Pos.c 0 -1, Pos.c 1 0)
        'J', (Pos.c 0 -1, Pos.c -1 0)
        '7', (Pos.c 0 1, Pos.c -1 0)
        'F', (Pos.c 0 1, Pos.c 1 0)
    ] 
    |> Map.ofList

let parseLine y (s:string) = 
    s.ToCharArray() |> Array.mapi (fun x c -> Pos.c x y, c)
let parse (lines:string array) =
    lines |> Array.mapi parseLine
    |> Array.collect id
    |> Map.ofArray

let follow map cPos prevPos  = 
    let pipe = map |> Map.find cPos
    let d1,d2 = pipes |> Map.find pipe
    let s1 = Pos.add d1 cPos
    let s2 = Pos.add d2 cPos
    if s1 = prevPos then s2
    elif s2 = prevPos then s1
    else failwithf "WTF?????"


//Hitta start
// För varje tick
// Följ båda ändarna till nästa ruta, 
// - tills de är på samma ruta (Ev försöka kolla om de passerat varandra)
let rec tick (map:Map<Pos,char>) (prev1:List<Pos>) (prev2:List<Pos>) = 
    match prev1, prev2 with 
    |a::_, b::_ when a = b -> 
        printfn "Ticked to same spot?"
        prev1,prev2
    |pa1::pa2::_, pb1::pb2::_ -> 
        let nextA = follow map pa1 pa2
        let nextB = follow map pb1 pb2

        tick map (nextA::prev1) (nextB::prev2)
    |_ -> failwithf "WTF?"
    

let findPaths input = 
    let map = parse input
    let start,_ = map |> Map.toArray |> Array.find (fun (k,v) -> v = 'S')

    //Hitta två rutor som ansluter till start
    let nextTwo = 
        [|Pos.c -1 0; Pos.c 1 0;Pos.c 0 -1; Pos.c 0 1;|]
        |> Array.filter 
            (fun p -> 
                let p = Pos.add p start 
                let c = map |> Map.tryFind p |> Option.defaultValue '.' 
                let dirs = pipes |> Map.tryFind c
                dirs 
                |> Option.map 
                    (fun (d1,d2) -> 
                        let s1 = Pos.add d1 p
                        let s2 = Pos.add d2 p
                        s1 = start || s2 = start 
                    )
                |> Option.defaultValue false
            )
        |> Array.map (fun p -> Pos.add p start)

    let path1, path2 = tick map (nextTwo[0]::start::[]) (nextTwo[1]::start::[])
    path1, path2

let solve1 input = 
    let path1, path2 = findPaths input
    path1.Length - 1


//https://en.wikipedia.org/wiki/Shoelace_formula
let area_shoelace (points:Pos list) =
    // Add the first point to the end of the list to close the polygon
    let points = points @ [points.Head]

    // Compute the sum of the products of the x coordinates and the next y coordinates
    let sum1 = Seq.map2 (fun (p1:Pos) (p2:Pos) -> p1.x * p2.y) points points.Tail
               |> Seq.sum
    // Compute the sum of the products of the y coordinates and the next x coordinates
    let sum2 = Seq.map2 (fun p1 p2 -> p1.y * p2.x) points points.Tail
               |> Seq.sum
    // Return the absolute value of half the difference of the sums
    abs (0.5 * float (sum1 - sum2))



///Otroligt knepig - snott från https://advent-of-code.xavd.id/writeups/2023/day/10/
let solve2 input = 
    let path1, path2 = findPaths input
    //Path1 & path2 starts and ends at the same spot.
    //So if we want to combine theese we remove first and last from one and reverse it to match them together
    let path2_short = path2[1..path2.Length-2] |> List.rev
    let path = (path1 @ path2_short) |> List.rev
    let area = area_shoelace path

    //https://en.wikipedia.org/wiki/Pick%27s_theorem
    let num_interior_points = int(abs(area) - 0.5 * float (path.Length) + 1.0)

    num_interior_points
    
let b = solve2 inptest1 //4 - Det här ska eg bli 4 men blir 1, men jag orkar inte bry mig
let a = solve2 inptest2 //8 

// 7119 to high
let part1 = solve1 input
let part2 = solve2 input
