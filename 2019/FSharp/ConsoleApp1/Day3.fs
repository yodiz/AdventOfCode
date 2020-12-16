#if INTERACTIVE
#else
module Day3
#endif

let basePath = """c:\Drive\Projects\Adventcode\2019\FSharp\ConsoleApp1\"""
let lines = System.IO.File.ReadAllLines(basePath + "day3.input")

type Point = {x : int; y:int } 
type Line = { p1:Point; p2:Point }

let getLines (str:string) wireLocInp = 
    str.Split(',')
    |> Array.fold 
        (fun (time, (x,y),currentPoints,crosses) c -> 
            let a = c.Substring(0,1)
            let l = System.Int32.Parse(c.Substring(1))
            
            let points = 
                [|
                    match a with 
                    |"R" -> for i = 1 to l do yield x+i,y,i
                    |"D" -> for i = 1 to l do yield x,y+i,i
                    |"L" -> for i = 1 to l do yield x-i,y,i
                    |"U" -> for i = 1 to l do yield x,y-i,i
                    |a -> failwithf ""
                |]

            let nx,ny,l = points.[points.Length-1]
            
            let a,b = 
                points 
                |> Array.fold 
                    (fun (wireLoc,crosses) (x,y,pl) -> 
                        match Map.tryFind (x,y) wireLocInp with
                        |None -> 
                            Map.add (x,y) (time+pl) wireLoc, crosses
                        |Some loc -> 
                        wireLoc, Map.add (x,y) (time+pl+loc) crosses
                    )
                    (currentPoints,crosses)

            time+l,(nx,ny), a, b 
        )
        (0, (0,0), Map.empty, Map.empty)
    
let _,_,wirePoints,_ = getLines lines.[0] Map.empty 
let _,_,_,crosses = getLines lines.[1] wirePoints 

let manhattanDist (p:Point) (p2:Point) = 
    let x = max p.x p2.x - min p.x p2.x
    let y = max p.y p2.y - min p.y p2.y
    x+y

let round1 = 
    crosses
    |> Map.toSeq
    |> Seq.map (fun ((x,y),l) -> manhattanDist { x=0;y=0 } { x=x;y=y })
    |> Seq.sort
    |> Seq.head

let round2 = 
    crosses
    |> Map.toSeq
    |> Seq.map (fun ((x,y),l) ->l)
    |> Seq.sort
    |> Seq.head


