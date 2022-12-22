#if INTERACTIVE
#load "../Common.fsx"
#else
module DayT
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

type Node<'a> = { Id : int; Data : 'a }    

type Vertex<'a> = {
    Item : 'a
}
let vertex i = { Item = i }

type Graph<'a when 'a : comparison> = {
    Vertices : Vertex<'a> list 
    Edges : Map<Vertex<'a>, Vertex<'a> list>
}
module Graph = 
    let empty = { Vertices = []; Edges = Map.empty }
    let addVertex (v) graph  = 
       { graph with Vertices = v::graph.Vertices }
    let addEdge from too graph = 
        { graph with Edges = graph.Edges 
                             |> Map.add from (too::(graph.Edges |> Map.tryFind from |> Option.defaultValue [])) }

let dijkstra<'a when 'a : comparison> (graph:Graph<'a>) (source:Vertex<'a>) (stopAt:Vertex<'a> option)= 
    let dist = System.Collections.Generic.Dictionary()
    let prev = System.Collections.Generic.Dictionary()
    let mutable Q = Set.empty
    for v in graph.Vertices do
        dist[v] <- 2000
        prev[v] <- None
        Q <- Q |> Set.add v 

    dist[source] <- 0

    while Q |> Set.isEmpty |> not do
        let u = Q |> Set.toSeq |> Seq.minBy (fun v -> dist[v])
        Q <- Q |> Set.remove u
        
        match stopAt with 
        |Some stopAt when stopAt = u -> 
            Q <- Set.empty
        |_ -> ()

        let neighbors = graph.Edges 
                        |> Map.tryFind u 
                        |> Option.defaultValue [] 
                        |> List.filter (fun x -> Q |> Set.contains x)

        for v in neighbors do
            let alt = dist[u] + 1 //Graph.edges u v graph
            if alt < dist[v] then
                dist[v] <- alt
                prev[v] <- Some u
            ()

    dist,prev


let getGraph (map:byte array array) = 
    let mutable graph = Graph.empty
    map
    |> Array.mapi (fun y yv-> 
        yv 
        |> Array.mapi (fun x xv -> 
            graph <- graph |> Graph.addVertex (vertex (x,y))

            [1,0; -1,0; 0,1; 0,-1]
            |> List.map (fun (dx,dy) -> 
                            let xx = x + dx
                            let yy = y + dy
                            if xx < 0 || xx >= map[0].Length || yy < 0 || yy >= map.Length then 
                                ()
                            elif (map[yy][xx]) <= map[y][x] + 1uy then
                                graph <- graph |> Graph.addEdge (vertex(x,y)) (vertex (xx,yy))
                            else ()
                        )
     
        )
    ) |> ignore
    graph


type State = {
    Map : byte array array
    Start : int*int
    Goal : int*int
}

let load file = 
    let mutable s = 0,0
    let mutable e = 0,0
    let parseLine y (str:string) = 
        str.ToCharArray() |> Array.mapi (fun x -> function |'S' -> s <- x,y; 'a' |'E' ->  e <- x,y; 'z' |a -> a) |> Array.map byte

    {Map = load folder file |> Array.mapi parseLine; Start = s; Goal = e}


let calc filename = 
    let l = load filename 
    let g = getGraph l.Map
    let (dist, path) =  dijkstra g (vertex (l.Start)) (Some (vertex (l.Goal)))
    dist[(vertex l.Goal)]
    
let calc2 filename = 
    let l = load filename 
    let g = getGraph l.Map
    //Reverse edges
    let e = 
        g.Edges
        |> Map.fold (fun s k v -> 
            v
            |> List.fold (fun s x -> s |> Map.add x (k :: (s |> Map.tryFind x |> Option.defaultValue []))) s
        ) Map.empty
    let g = { g with Edges = e }
    //calc from goal to all others
    let (dist, path) =  dijkstra g (vertex (l.Goal)) None

    // get dist for all a's and take min
    let a = l.Map |> Array.mapi (fun y i -> i |> Array.mapi (fun x v -> 
                                                                let b = v = byte 'a'
                                                                if b then Some (x,y)
                                                                else None
                                                            ))
            |> Array.collect (fun x -> x |> Array.choose id)

    let xx = 
        a
        |> Array.mapi 
            (fun i (x,y) -> 
                let d = dist[vertex (x,y)]
                printfn "Calc %i of %i (%i,%i) = %i" i a.Length x y d
                d
            )
        |> Array.min
    xx

let test1 = AoC.Test.equal "Test1" 31 (calc "test1.txt")
let test2 = AoC.Test.equal "Test2" 29 (calc2 "test1.txt")

let part1 = calc "input.txt"
let part2 = calc2 "input.txt"


//
//printfn "%i" part1
//printfn "%i" part2


