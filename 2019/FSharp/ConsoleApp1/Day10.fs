#if COMPILED
module Day10
#endif

let rec gcd x y =
    if y = 0 then x
    else gcd y (x % y)

let buildDirs (w:int) (h:int) = 
    printfn "Generating dirs"
    let a = 
        [for x = -w to w do
          for y = -h to h do
           match gcd x y with
           |0|1 -> yield x,y
           |d -> yield x/(abs d),y/ (abs d)
        ]
        |> Set.ofList
        |> Set.toList
        |> List.sortBy (fun (x,y) -> y,x)
    a

buildDirs 5 5





type Board = {
    Grid : bool[][]
    Width : int
    Height : int
}

let input = 
    let lines = System.IO.File.ReadAllLines("""c:\Drive\Projects\Adventcode\2019\FSharp\ConsoleApp1\Day10.input""")
    let a = lines |> Array.map (fun x -> x.ToCharArray() |> Array.map (function |'.' -> false |'#' -> true))
    {
        Grid = a
        Width = lines.[0].Length
        Height = lines.Length
    }

let rec trace (b:Board) (dx,dy) (px,py)  = 
    let x = px + dx
    let y = py + dy
    if x < 0 || y < 0 || x >= b.Width || y >= b.Height || (dx = 0 && dy = 0) then None
    else
        if b.Grid.[y].[x] = true then 
            Some (x,y)
        else
            trace b (dx,dy) (x,y)       

let buildDetect (b:Board)  = 
    let dirs = buildDirs b.Width b.Height

    let detect  (x,y) = 
        dirs
        |> List.map (fun d -> trace b d (x,y))
        |> List.fold (fun s x -> match x with |None -> s |Some _ -> s+1) 0 
    detect
    
let totalAsteroids = 
    input.Grid |> Array.collect (fun x -> x)
    |> Array.where ((=)true) |> Array.length


buildDirs input.Width input.Height

let round1 = 
    let detect  = buildDetect input
    input.Grid
    |> Array.mapi 
        (fun y cols -> 
            cols 
            |> Array.mapi 
                (fun x v -> 
                    (x,y),if v then detect (x,y) else 0
                )
        )
    |> Array.collect id
    |> Array.maxBy snd
    |> snd

let rec rotBeam (w,h) (ox,oy) (dx, dy) beamAt = 
    if dx = 0 && dy = 0 then 
        rotBeam (w,h) (ox,oy) (dx, dy - 1) beamAt
    else
        let x = ox+dx 
        let y = oy+dy

        if y < 0 then rotBeam (w,h) (ox,oy) (dx+1,dy) beamAt
        if x >= w then failwithf "test"
        else
            beamAt (x,y)
            rotBeam (w,h) (ox,oy) (dx,dy-1) beamAt

rotBeam (10,10) (5,5) (0,0) (fun (x,y) -> printfn "%i,%i" x y)


(0,-1)
(0,-2)