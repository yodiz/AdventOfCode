#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC



let parse (str:string) = 
    str.Substring(0,1).[0], Parse.int32 (str.Substring(1))
type Ship = { x:int;y:int;dir:char }
 
let dir c v (ship:Ship) = 
    match c with
    |'N' -> { ship with y = ship.y - v }
    |'S' -> { ship with y = ship.y + v }
    |'W' -> { ship with x = ship.x - v }
    |'E' -> { ship with x = ship.x + v }
    |a -> failwithf "asd %A" a

let diri = [|'N';'E';'S';'W'|]
 
let turnBy dir by =
    let by = by / 90
    ((diri |> Array.findIndex((=)dir)) + by) % diri.Length
    |> (fun x -> if x < 0 then diri.[diri.Length + x] else diri.[x])

let move (c,v) (ship:Ship) =
    match c with
    |'F' -> dir ship.dir v ship
    |'L' -> { ship with dir = turnBy ship.dir (0-v) }
    |'R' -> { ship with dir = turnBy ship.dir v }
    |a -> dir a v ship

let part1 = 
    let start = { x=0;y=0;dir='E' }
    load "Day12/input.txt" |> Array.map parse
    |> Array.fold
        (fun s x -> 
            let a = move x s; 
            a)
        start
    |> (fun f -> abs (start.x - f.x) + abs (start.y - f.y))

let rotateRight (x:int,y:int) = y* -1,x
let rotate deg (x:int,y:int) = 
    (deg) % 360
    |> function |90  | -270 -> rotateRight
                |180 | -180 -> rotateRight >> rotateRight
                |270 | -90  -> rotateRight >> rotateRight >> rotateRight
                |e -> failwithf "error %i" e
    |> (fun f -> f (x,y))

let move2 (c,v) (ship:Ship) (x,y) =
    match c with
    |'F' -> ship, (x + ship.x * v,y + ship.y * v)
    |'L' -> let (nx,ny) = rotate (v* -1) (ship.x, ship.y) in { ship with x = nx; y=ny }  , (x,y)
    |'R' -> let (nx,ny) = rotate (v) (ship.x, ship.y) in { ship with x = nx; y=ny }  , (x,y)
    |a -> dir a v ship, (x,y)

let part2 = 
    let start = { x=10;y= -1;dir='E' }
    load "Day12/input.txt" |> Array.map parse
    |> Array.fold
        (fun (w,s) x -> 
            let w2,s2 = move2 x w s; 
            //printfn "%A - %i,%i - %A" x w2.x w2.y s2
            (w2,s2))
        (start,(0,0))
    |> (fun (w,(x,y)) -> abs x + abs y)
    
    

   