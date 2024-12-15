#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let test1  = load folder "test1.txt"
    
let rec tryFindStr remainingStr pos dir (map:Pos.Map) = 
    match remainingStr with 
    |[] -> true
    |a::rest -> 
        let c = map.Map |> Map.tryFind pos |> Option.defaultValue '.'
        if c = a then 
            tryFindStr rest (Pos.add pos dir) dir map
        else false

let p1 a = 
    let map = Pos.Map.parse '.' a
    
    let dirs = 
        [Pos.Dir.East; Pos.Dir.West; Pos.Dir.North; Pos.Dir.South;
         Pos.create -1 -1;Pos.create 1 1;Pos.create 1 -1;Pos.create -1 1
        ]

    [0..map.Width*map.Height] 
    |> List.collect 
        (fun i -> 
            let x = i % map.Height
            let y = i / map.Width
            let p = Pos.create x y
            dirs 
            |> List.filter (fun d -> tryFindStr ['X'; 'M';'A';'S'] p d map)
        )
    |> List.length

let p2 a = 
    let map = Pos.Map.parse '.' a
    
    let dirs = 
        [
         Pos.create -1 -1;Pos.create 1 1;Pos.create 1 -1;Pos.create -1 1
        ]

    [0..map.Width*map.Height] 
    |> List.filter 
        (fun i -> 
            let x = i % map.Height
            let y = i / map.Width
            let p = Pos.create x y


            let isA = map.Map |> Map.tryFind p |> Option.defaultValue '.' = 'A'
            let d1 = (Pos.create 1 1)
            let d2 = (Pos.create -1 1)
            let p1 = p |> Pos.add (Pos.create -1 -1)
            let p2 = p |> Pos.add (Pos.create 1 -1)

            let k1 = tryFindStr ['M';'A';'S'] p1 d1 map || tryFindStr ['S';'A';'M'] p1 d1 map
            let k2 = tryFindStr ['M';'A';'S'] p2 d2 map || tryFindStr ['S';'A';'M'] p2 d2 map

            let b = isA && k1 && k2
            if b then
                printfn "Found pos %i,%i , P1= %i,%i, P2= %i,%i" x y p1.x p1.y p2.x p2.y
            b

        )
    |> List.length
Test.equal "Test1" 18 (p1 test1)
Test.equal "Test2" 9 (p2 test1)

let part1 = p1 input
let part2 = p2 input

printfn "Part1: %i" part1
printfn "Part2: %i" part2
