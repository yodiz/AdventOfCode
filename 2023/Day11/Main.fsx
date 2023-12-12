#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"
let inptest2  = load folder "test2.txt"


let parse y (line:string) = 
    line.ToCharArray()
    |> Array.mapi (fun x c -> match c with |'#' -> Some (Pos.create x y) |_ -> None )
    |> Array.choose id

let solve1 input =
    let galaxy = 
        input 
        |> Array.mapi parse
        |> Array.collect id
        |> Set.ofArray
    let width, height = 
        galaxy |> Set.fold (fun (mx,my) x -> (max mx x.x), (max my x.y)) (0,0)

    //För varje kolumn, kolla igenom raderna och se om kolumen är tom. Spara ner en lita med tomma kolumner
    let emptyCols = 
        [0..width]
        |> List.map (fun x -> [0..height] |> List.forall (fun y -> galaxy |> Set.contains (Pos.create x y) |> not))
        |> List.mapi (fun i x -> if x then Some i else None)
        |> List.choose id

    let emptyRows = 
        [0..height]
        |> List.map (fun y -> [0..width] |> List.forall (fun x -> galaxy |> Set.contains (Pos.create x y) |> not))
        |> List.mapi (fun i x -> if x then Some i else None)
        |> List.choose id


    //Plussa på x för varje tom column 
    let galaxyExpanded = 
        galaxy
        |> Set.map 
            (fun p -> 
                let inc = emptyCols |> List.filter (fun x -> x < p.x) |> List.length
                { p with x = p.x + inc }
            )
        |> Set.map 
            (fun p -> 
                let inc = emptyRows |> List.filter (fun y -> y < p.y) |> List.length
                { p with y = p.y + inc }
            )

    
    let a = galaxyExpanded |> Set.toArray
    let pairs = 
        Array.allPairs a a
        |> Array.map (fun (a,b) -> min a b, max a b)
        |> Array.filter (fun (a,b) -> a <> b)
        |> Array.distinct

    let dist (a:Pos) (b:Pos) = abs(a.x - b.x) + abs(a.y - b.y)

    pairs |> Array.map (fun (a,b) -> dist a b)
    |> Array.sum




let solve2 input = 
    0

let aTest = solve1 inptest1

let part1 = solve1 input
