#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day6
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"

let parse (input:string array) = 
    let time = (Text.split_noempty " " input[0])[1..] |> Array.map Parse.int32
    let d = (Text.split_noempty " " input[1])[1..] |> Array.map Parse.int32
    Array.map2 (fun t d -> {|Time=t;Distance=d|}) time d

let parse2 (input:string array) = 
    let time = (Text.split2 ":" input[0]) |> snd |> (fun x -> x.Replace(" ", "")) |> Parse.int64
    let d = (Text.split2 ":" input[1]) |> snd |> (fun x -> x.Replace(" ", "")) |> Parse.int64
    {|Time=time;Distance=d|}    
    
let calcRaceTime (buttonPressTime:int64) (time:int64) = 
    let speed = buttonPressTime
    if speed = 0L then System.Int64.MaxValue
    else time * speed

let findBetter time distance = 
    [|1L..time|]
    |> Array.map (fun t -> calcRaceTime t (time-t))
    |> Array.filter (fun r -> r > distance)
    |> Array.length

let solve1 input = 
    parse input
    |> Array.map (fun x -> findBetter x.Time x.Distance)
    |> Array.reduce (fun s x -> s*x)

   
let solve2 input = 
    parse2 input
    |> (fun x -> findBetter x.Time x.Distance)
   
let aTest = solve1 inptest1
let aTest2 = solve2 inptest1
let part1 = solve1 input 
let part2 = solve2 input
