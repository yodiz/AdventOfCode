#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

let rec lowHigh (low:int) (high:int) (lowChar:char) (highChar:char) chars =   
    match chars with
    |[] -> low
    |char :: tail -> 
        if char = lowChar then
            lowHigh low (high - ((high - low) / 2) - 1) lowChar highChar tail
        elif char = highChar then
            lowHigh (high - ((high - low) / 2)) high lowChar highChar tail
        else failwithf "Unexpected char %A" char

let parseLine (str:string) =    
    let row = lowHigh 0 127 'F' 'B' (str.Substring(0,7).ToCharArray() |> Array.toList)
    let col = lowHigh 0 7 'L' 'R' (str.Substring(7,3).ToCharArray()  |> Array.toList)
    {| Row =  row; Col = col; Id = row * 8 + col |}

let part1 =     
    load "Day5/input.txt" 
    |> Array.map (parseLine)
    |> Array.map (fun x -> x.Id)
    |> Array.max

let part2 = 
    load "Day5/input.txt" 
    |> Array.map (parseLine)
    |> Array.map (fun x -> x.Id)
    |> Array.sort
    |> Array.reduce (fun p n -> if p + 1 = n then n else p)
    |> (fun x -> x+1)

