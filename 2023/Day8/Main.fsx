#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

type NodeLookup = Map<string,NodeL> -> NodeL
and NodeL = { LeftL : NodeLookup; Value : string; RightL : NodeLookup }
type Node = { WalkLeft : Lazy<Node>; Value:string; WalkRight : Lazy<Node> }

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = loadAll folder "input.txt"
let inptest1  = loadAll folder "test1.txt"
let inptest2  = loadAll folder "test2.txt"

//AAA = (BBB, CCC)
let parseNode (s:string) = 
    let id, lr = Text.split2 "=" s
    let l,r = Text.split2 "," lr
    let t = Text.trimc [|' ';'(';')'|]
    let x str s = s |> Map.find  str
    t id, {LeftL = x (t l); Value = id.Trim(); RightL = x (t r)}

let memo key fn = 
    let mutable l = Map.empty
    (fun x -> 
        let k = key x
        match l |> Map.tryFind k with
        |Some v -> v
        |None -> 
            let v = fn x
            l <- l |> Map.add k v
            v
    )

let parse (s:string) =
    let input, nodes = Text.split2 "\r\n\r\n" s

    let nodes = Text.split "\r\n" nodes |> Array.map parseNode 
    let nodesMap = nodes |> Map.ofArray
    let rec nodeL2Node (n:NodeL) = 
        lazy (
            { WalkLeft = n.LeftL nodesMap |> nodeL2Node; 
              Value = n.Value; 
              WalkRight = n.RightL nodesMap |> nodeL2Node }
        )

    let nodes = 
        nodes 
        |> Array.map (fun (id, n)-> nodeL2Node n)
    
    input.ToCharArray(), nodes


let rec walk (i:int) (input:char array) (node:Node) = 
    // if i > input.Length * 2 then failwithf "Input length %i" input.Length
    if node.Value = "ZZZ" then i
    else
        let d = input[i % input.Length]
        let nextNode = 
            match d with 
            |'L' -> node.WalkLeft.Value
            |'R' -> node.WalkRight.Value
        // if i % 100000000 = 0 then                       
        // printfn "%i(%i) %s - Walking %c" i (i % input.Length) node.Value d
        walk (i+1) input nextNode

let solve1 input = 
    let inp, nodes = parse input
    let start = nodes |> Array.find (fun x -> x.Value.Value = "AAA")
    walk 0 inp start.Value

let solve2 input = 
    let inp, nodes = parse input
    let start = nodes |> Array.filter (fun x -> x.Value.Value.EndsWith("A"))

    
    walk 0 inp start.Value

Test.equal "Test1" 2 (solve1 inptest1)
Test.equal "Test2" 6 (solve1 inptest2)
let aTest = solve1 inptest1
let aTest2 = solve1 inptest2

let part1 = solve1 input 
// let part2 = solve2 input
