#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC


let parse y (x:string) = 
    x.ToCharArray() 
    |> Array.mapi (fun x c -> (x,y,0),match c with |'.' -> false |'#' -> true)
    
let init str = 
    load str
    |> Array.mapi parse
    |> Array.collect id 
    |> Array.filter snd
    |> Array.map fst
    |> Set.ofArray

let getNeighburs  (x:int,y:int,z:int) = 
    seq {
        for x1 = (x-1) to x + 1 do
            for y1 = (y-1) to y + 1 do
                for z1 = (z-1) to z + 1 do
                    if not (x1 = x && y1 = y && z1 = z) then
                        x1,y1,z1
    } 


let cycle (state:Set<int*int*int>) = 
    let getActiveNeighbursCount (x,y,z) = 
        getNeighburs (x,y,z)
        |> Seq.fold (fun i x -> state |> Set.contains x |> function |true -> i+1 |false -> i) 0

    let inactiveToActivate = 
        state |> Set.toSeq |> Seq.collect getNeighburs
        |> Set.ofSeq
        |> Set.filter (fun x -> getActiveNeighbursCount x |> function |3 -> true |_ -> false)

    let activeToActive = 
        state
        |> Set.filter (fun x -> getActiveNeighbursCount x |> function |2 |3 -> true |_ -> false )
        
    Set.union inactiveToActivate activeToActive

let rec runCycles i m c = 
    if i = m then c
    else 
        printfn "Running %i" i
        runCycles (i+1) m (cycle c)

let test1 = init "Day17/test1.txt" |> runCycles 0 6 |> Set.count |> Test.equal "Test1" 112
let part1 = init "Day17/input.txt" |> runCycles 0 6 |> Set.count 
    

module Part2 =
    let parse y (x:string) = 
        x.ToCharArray() 
        |> Array.mapi (fun x c -> (x,y,0,0),match c with |'.' -> false |'#' -> true)
    
    let init str = 
        load str
        |> Array.mapi parse
        |> Array.collect id 
        |> Array.filter snd
        |> Array.map fst
        |> Set.ofArray

    let getNeighburs  (x:int,y:int,z:int,w:int) = 
        seq {
            for x1 = (x-1) to x + 1 do
                for y1 = (y-1) to y + 1 do
                    for z1 = (z-1) to z + 1 do
                        for w1 = (w-1) to w + 1 do
                        if not (x1 = x && y1 = y && z1 = z && w1 = w) then
                            x1,y1,z1,w1
        } 


    let cycle (state:Set<int*int*int*int>) = 
        let getActiveNeighbursCount (x,y,z,w) = 
            getNeighburs (x,y,z,w)
            |> Seq.fold (fun i x -> state |> Set.contains x |> function |true -> i+1 |false -> i) 0

        let inactiveToActivate = 
            state |> Set.toSeq |> Seq.collect getNeighburs
            |> Set.ofSeq
            |> Set.filter (fun x -> getActiveNeighbursCount x |> function |3 -> true |_ -> false)

        let activeToActive = 
            state
            |> Set.filter (fun x -> getActiveNeighbursCount x |> function |2 |3 -> true |_ -> false )
        
        Set.union inactiveToActivate activeToActive

    let rec runCycles i m c = 
        if i = m then c
        else 
            printfn "Running %i" i
            runCycles (i+1) m (cycle c)

    let test2 = init "Day17/test1.txt" |> runCycles 0 6 |> Set.count |> Test.equal "Test2" 848
    let part2 = init "Day17/input.txt" |> runCycles 0 6 |> Set.count 
    



module Part3 =
    let parse y (x:string) = 
        x.ToCharArray() 
        |> Array.mapi (fun x c -> (x,y,0,0),match c with |'.' -> false |'#' -> true)
    
    let init str = 
        load str
        |> Array.mapi parse
        |> Array.collect id 
        |> Array.filter snd
        |> Array.map fst
        |> Set.ofArray

    let getNeighburs  (d:int list) = 
        d
        |> List.foldi (fun i x -> d |> list |> map)
        seq {
            for x1 = (x-1) to x + 1 do
                for y1 = (y-1) to y + 1 do
                    for z1 = (z-1) to z + 1 do
                        for w1 = (w-1) to w + 1 do
                        if not (x1 = x && y1 = y && z1 = z && w1 = w) then
                            x1,y1,z1,w1
        } 


    let cycle (state:Set<int*int*int*int>) = 
        let getActiveNeighbursCount (x,y,z,w) = 
            getNeighburs (x,y,z,w)
            |> Seq.fold (fun i x -> state |> Set.contains x |> function |true -> i+1 |false -> i) 0

        let inactiveToActivate = 
            state |> Set.toSeq |> Seq.collect getNeighburs
            |> Set.ofSeq
            |> Set.filter (fun x -> getActiveNeighbursCount x |> function |3 -> true |_ -> false)

        let activeToActive = 
            state
            |> Set.filter (fun x -> getActiveNeighbursCount x |> function |2 |3 -> true |_ -> false )
        
        Set.union inactiveToActivate activeToActive

    let rec runCycles i m c = 
        if i = m then c
        else 
            printfn "Running %i" i
            runCycles (i+1) m (cycle c)

    let test2 = init "Day17/test1.txt" |> runCycles 0 6 |> Set.count |> Test.equal "Test2" 848
    let part2 = init "Day17/input.txt" |> runCycles 0 6 |> Set.count 
    