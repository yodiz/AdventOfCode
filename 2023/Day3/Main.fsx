#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day2
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"
let inptest2  = load folder "test2.txt"

type Pos = { X : int; Y : int } 
type Op = { Pos: Pos; Op : char}
type Num = { Pos : Pos list; N : int }

let rec parseLine' (y:int) (x:int) (l:string) curNumber numbers operations = 
    let cn =
        match curNumber with 
        |Some (number:string) -> 
            let poss = number.ToCharArray() |> Array.mapi (fun i _ -> {X=x-i-1; Y=y}) |> Array.toList
            Some { Pos = poss; N = Parse.int32 number }
        |None -> None
    let cnumbers = (match cn with |None -> numbers |Some cn -> cn::numbers)
    if x = l.Length then 
        cnumbers, operations 
    else    
        match l[x] with
        |'.' -> parseLine' y (x+1) l None cnumbers operations
        |c when System.Char.IsNumber c -> 
            let n = 
                match curNumber with 
                |None -> c.ToString()
                |Some s -> s + (c.ToString())
            parseLine' y (x+1) l (Some n) numbers operations
        |op -> 
            parseLine' y (x+1) l None cnumbers ({ Pos = {X=x; Y=y}; Op = op }::operations)

let parseLine y l = parseLine' y 0 l None [] []

let common input = 
    let nums, ops = 
        input 
        |> Array.mapi (fun y l -> parseLine y l)
        |> Array.toList
        |> List.fold (fun (al,bl) (x,y) -> List.append al x, List.append bl y) ([],[])

    let nums = 
        nums
        |> List.collect (fun x -> x.Pos |> List.map (fun p -> p,x))
        |> Map.ofList

    //För vaje op, hitta dess grannar
    let findNeighbors (p:Pos) = 
        // printfn "%i,%i" p.X p.Y
        [-1,-1; 0,-1; 1,-1
         -1 ,0;       1 ,0
         -1 ,1; 0, 1; 1 ,1
        ]
        |> List.choose 
            (fun (dx,dy) -> 
                nums 
                |> Map.tryFind { X=p.X+dx;Y=p.Y+dy }
            )
        |> List.distinct
    ops
    |> List.map (fun op -> 
        let ns = findNeighbors op.Pos
        op,ns)

let solve1 input = 
    common input
    |> List.collect (fun (op,n) -> n)
    |> List.distinct
    |> List.sumBy (fun (n) -> n.N)
    
    
let solve2 input = 
    common input
    |> List.filter (fun (op,n) -> op.Op='*' && n.Length = 2)
    |> List.map (fun (op,n) -> n[0].N*n[1].N)
    |> List.sum
    

Test.equal "" 4361 (solve1 inptest1)

let aTest = solve1 inptest1
let part1 = solve1 input
let part2 = solve2 input
