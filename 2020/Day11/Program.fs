#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

type Seat = |Empty|Floor|Full
let parseLine (str:string) = 
    str.ToCharArray() |> Array.map (function |'L' -> Empty |'#' -> Full |'.' -> Floor)

let directions = 
    [|(-1,-1); (0,-1); (1,-1);
      (-1,0);          (1,0);
      (-1,1);  (0,1);  (1,1);|]

let checkSeat (b:Seat array array) x y = 
    if x < 0 || y < 0  || x >= b.[0].Length || y >= b.Length then None
    else Some (b.[y ].[x])

let rule1 (b:Seat array array) x y s = 
    let adj = directions |> Array.sumBy (fun (dx, dy) -> if checkSeat b (x+dx) (y+dy) = Some Full then 1 else 0)
    match s, adj  with
    |Seat.Empty,0 -> Seat.Full
    |Seat.Full, a when a >= 4 -> Seat.Empty
    |x,_ -> s

let solve rule b = 
    let rec solve (b:Seat array array) =
        let nb = b |> Array.mapi (fun y r -> r |> Array.mapi (fun x s -> rule b x y s))
        if b = nb then b
        else solve nb
    solve (b |> Array.map parseLine)
    |> Array.fold (fun s r -> 
        r |> Array.fold (fun s -> function |Full -> s + 1 |_ -> s) s) 0

let test1 = load "Day11/Test1.txt"|> solve rule1
let part1 = load "Day11/input.txt" |> solve rule1

let rec look (b:Seat array array) (x,y) (dx,dy) = 
    let x = x + dx in let y = y + dy in
    match checkSeat b x y with
    |Some Floor -> look b (x,y) (dx,dy)
    |Some Full -> 1 |_ -> 0

let rule2 (b:Seat array array)  x y s = 
    let adj2 = directions |> Array.sumBy (look b (x,y))
    match s, adj2  with
    |Seat.Empty,0 -> Seat.Full
    |Seat.Full, a when a >= 5 -> Seat.Empty
    |x,_ -> x

let test2 = load "Day11/Test1.txt" |> solve rule2
let part2 = load "Day11/input.txt" |> solve rule2