#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

type Tile = {
    Id : int64
    Map : bool array array
}
    with 
        member x.Edges = 
            let top = x.Map.[0]
            let bottom = x.Map.[x.Map.Length - 1] |> Array.rev
            let left = x.Map |> Array.map (fun x -> x.[0]) |> Array.rev
            let right = x.Map |> Array.map (fun x -> x.[x.Length - 1])
            [|top;right;bottom;left|]

        member x.Rotate = 
            let n = x.Map.Length
            let r = 
                x.Map
                |> Array.mapi 
                    (fun i row -> 
                        row 
                        |> Array.mapi (fun j v -> x.Map.[n - j - 1].[i])
                    )

            {
                Id = x.Id
                Map = r
            }
        member x.Flip = 
            {
                Id = x.Id
                Map = x.Map |> Array.map Array.rev
            }            
        





let parseTile (str:string) = 
    let lines = str |> Text.split "\r\n"
    let id = lines.[0].Substring(5).TrimEnd(':') |> Parse.int64
    let map = 
        lines.[1..] 
        |> Array.map (fun x -> x.ToCharArray() |> Array.map (function |'.' -> false |'#' -> true))
        
    { Id = id; Map = map }



let findEdge (tiles:Tile array) id edge = 
    let e = edge |> Array.rev
    tiles
    |> Array.tryPick 
        (fun t -> 
            if t.Id = id then None
            else
                t.Edges
                |> Array.foldi 
                    (fun i s x -> if x = e || x = edge then Some (i,t) else s) 
                    None
                
        )

let load (str:string) = 
    let tiles =
        loadAll str |> Text.split "\r\n\r\n"
        |> Array.map parseTile

    
    tiles
    |> Array.filter
        (fun t -> 
            let n = 
                t.Edges 
                |> Array.mapi (fun i e -> match findEdge tiles t.Id e with |Some s -> true |none -> false) 
                |> Array.filter id
                |> Array.length

            n = 2
        )
    |> Array.map (fun t -> t.Id)
    |> Array.reduce (fun x y -> x * y)





let test1 = load "Day20/test1.txt" |> Test.equal "Test1" 20899048083289L
let part1 = load "Day20/input.txt"







let load2 (str:string) = 
    let tiles =
        loadAll str |> Text.split "\r\n\r\n"
        |> Array.map parseTile

    let corners = 
        tiles
        |> Array.filter
            (fun t -> 
                let n = 
                    t.Edges 
                    |> Array.mapi (fun i e -> match findEdge tiles t.Id e with |Some s -> true |None -> false) 
                    |> Array.filter id
                    |> Array.length

                n = 2
            )
    

    let start = corners.[0]
    let mutable last = start

    printfn "Test #3"

    let found = Array.init tiles.Length (fun _ -> Array.init tiles.Length (fun _ -> start))


    for i = 0 to tiles.Length - 1 do
        let y = i / 3
        let x = i % 3

        printfn "(%i,%i)" x y
                
        
        if i = 0 then 
            //Rotate until edge 0 and edge 3 is not found on other tiles                        
            while findEdge tiles last.Id last.Edges.[0] |> Option.isSome || findEdge tiles last.Id last.Edges.[3] |> Option.isSome do
                last <- last.Rotate    
            printfn "%A" last
        else
            let ly = (i-1) / 3
            let lx = (i-1) % 3

            if ly = y then
                printfn "ly = y"
                let toComp = found.[x-1].[y]
                //Find tile that has edge matching right side of last
                match findEdge tiles toComp.Id toComp.Edges.[1] with
                |None -> failwithf "Did not find thingy"
                |Some (e,t) -> 
                    let toRot = 
                        if e = 0 then 3
                        elif e = 1 then 2
                        elif e = 2 then 1
                        elif e = 3 then 0
                        else failwithf ""
                    last <- t
                    for r = 0 to toRot - 1 do  
                        last <- last.Rotate
            else
                //Find tile that has edge matching bottom side of tile above
                let toComp = found.[x].[y-1]
                match findEdge tiles toComp.Id toComp.Edges.[2] with
                |None -> 
                    
                    failwithf "Did not find matching edge for "
                |Some (e,t) -> 
                    let toRot = 
                        if e = 0 then 0
                        elif e = 1 then 3
                        elif e = 2 then 2
                        elif e = 3 then 1
                        else failwithf ""
                    last <- t
                    for r = 0 to toRot - 1 do  
                        last <- last.Rotate
                        
                    
        printfn "Tile %i,%i <- %i" x y last.Id        
        found.[x].[y] <- last

    found
    


let test2 = load2 "Day20/test1.txt" 