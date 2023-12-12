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

let dirs = [0,1; 0,-1; 1,0; -1,0] |> List.map (fun (x,y) -> Pos.c x y)


// [<TailCall>]
let rec goNext i visited (currentPos:Pos) map pipes toWalk = 

    let visited = (Set.add currentPos visited)
    if i > 15000  then 
        printfn "Walking: %i,%i" currentPos.x currentPos.y
        Set.empty
    else
        // 
        //Find Neighbors can walk to
        let toWalk = 
            dirs 
            |> List.map (fun p -> Pos.add currentPos p)
            |> List.filter (fun p -> visited |> Set.contains p |> not) //Exclude visited
            |> List.filter (fun p -> toWalk |> Set.contains p |> not) //Exclude planned
            |> List.filter (fun p -> p.x > -2 && p.y > -2 && p.x < 150 && p.y < 150)
            |> List.filter 
                (fun p -> 
                    let isPipe = pipes |> Set.contains p 
                    let x = map |> Map.tryFind p |> Option.defaultValue '.'
                    if isPipe then 
                        match x with |'.' -> true |_ -> false
                    else true
                )
        match t
        match toWalk with
        |[] -> visited
        |toWalk -> 


            goNext (i+1) visited nextPos map pipes

            toWalk
            |> List.fold 
                (fun s nextPos -> 
                    

                    // printfn "To Walk : %A" nextPos
                    let a = goNext (i+1) s nextPos map pipes
                    Set.union a s
                )
                visited

    //



    
let solve2 input = 
    let path1, path2 = findPaths input
    let pipes = (path1 @ path2) |> Set.ofList
    let map = parse input

    let freeSpots = goNext 0 Set.empty (Pos.c -1 -1) map pipes Set.empty


    //Börja utnaför kartan
    //  - Markera  ruta som utanför
    //  - Kolla vad vi är på för ruta (Räkna bara pipes som är ansluten) och går vidare på tillåtna rutor... och repetera
    //  - Om ruta man vill gå till redan är markerad så struntar vi i det hållet
    0

// Test.equal "Test1" 114 (solve1 inptest1)
// let aTest = solve1 inptest1
// let part1 = solve1 input 
// let part2 = solve2 input


// let a = solve2 inptest2
