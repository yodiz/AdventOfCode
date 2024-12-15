#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let test1  = load folder "test1.txt"
//let test2  = load folder "test2.txt"
    
type A = |Looping |Oob of Set<Pos>

let rec tickUntilOutOfBounds (pos:Pos) direction (map:Pos.Map) visited visitedAndDir = 
    if visitedAndDir |> Set.contains (pos,direction) then Looping 
    elif pos.x < 0 || pos.x >= int64 map.Width || pos.y < 0 || pos.y >= int64 map.Height
    then Oob visited
    else 
        let next = pos |> Pos.add direction
        let nextSpotFree = map.Map |> Map.tryFind next |> Option.defaultValue '.' |> function |'.' -> true |'#' -> false |a -> failwithf "#1"
        if nextSpotFree then
            tickUntilOutOfBounds next direction map (visited |> Set.add next) (visitedAndDir |> Set.add (pos, direction))
        else
            let rotateDir = Pos.Dir.turnRight direction
            tickUntilOutOfBounds pos rotateDir map visited visitedAndDir

let p1 a = 
    let map = Pos.Map.parse '.' a
    let startPos,_ = map.Map |> Map.toSeq |> Seq.find (fun (p,v) -> v = '^') 
    let map = { map with Map = map.Map |> Map.remove startPos }

    let v = tickUntilOutOfBounds startPos Pos.Dir.North map Set.empty Set.empty |> function |Oob v -> v |Looping -> failwithf "Looping unexpected"
    Set.count (v) - 1


//Bruteforcead :(
let p2 a = 
    let map = Pos.Map.parse '.' a
    let startPos,_ = map.Map |> Map.toSeq |> Seq.find (fun (p,v) -> v = '^') 
    let map = { map with Map = map.Map |> Map.remove startPos }
    let mutable c = 0
    let mutable p = 0
    //För varje ruta
    // Om ledig placera ett object och testa om den loopar, om så spara
    for x = 0 to map.Width - 1 do
        for y = 0 to map.Height - 1 do
        let empty = map.Map |> Map.tryFind (Pos.create x y) |> Option.defaultValue '.'  = '.'
        if empty then
            if p < x then 
                printfn "%i,%i - %i" x y c
                p <- x
            let v = tickUntilOutOfBounds startPos Pos.Dir.North { map with Map = map.Map |> Map.add (Pos.create x y) '#' } Set.empty Set.empty
            let looping = match v with |Looping -> true |Oob _ -> false
            if looping then 
                c <- c + 1
            ()
        else ()
            

    c

Test.equal "Test1" 41 (p1 test1)
Test.equal "Test2" 6 (p2 test1)

let part1 = p1 input
let part2 = p2 input

printfn "Part1: %i" part1
printfn "Part2: %i" part2
