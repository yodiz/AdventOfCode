#if INTERACTIVE
#load "../Common.fsx"
#else
module DayT
#endif

open Microsoft.FSharp.Core.Operators.Checked
open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

type Vec2D = { x : int; y: int }
module Vec2D = 
    let manhattanDistance (v1:Vec2D) (v2:Vec2D) = abs(v1.x - v2.x) + abs(v1.y - v2.y)
    let apply (v1:Vec2D) (v2:Vec2D) fn = { x = fn v1.x v2.x; y = fn v1.y v2.y }
    let add v1 v2 = apply v1 v2 (+)
    let multiply v1 v2 = apply v1 v2 (*)
    let subtract v1 v2 = apply v1 v2 (-)
let vec2D x y = { x = x; y = y }

type Sensor = { Loc:Vec2D; NearestBeacon:Vec2D; Radius : int }
//Sensor at x=20, y=1: closest beacon is at x=15, y=3
module Parse = 
    open Parser
    let parseLine (str:string) = 
        let pCord = string "x=" >>. int32 .>> string ", y=" .>>. int32 |> map (fun (x,y) -> vec2D x y)
        string "Sensor at " >>. pCord .>> string ": closest beacon is at " .>>. pCord .>> eos
        |> Parser.runOrFail str
        |> (fun (sens, beac) -> { Loc = sens; NearestBeacon = beac; Radius = Vec2D.manhattanDistance sens beac })

let segmentOnLine (y: int32) (sensor: Sensor) : Option<int32 * int32> =
    let dy = abs (sensor.Loc.y - y)
    let dx = (sensor.Radius - dy)
    if dx < 0 then None
    else
        let x1 = sensor.Loc.x - dx
        let x2 = sensor.Loc.x + dx
        Some((x1, x2))

let rec mergeSegments (segments: (int * int) list) =
    match segments with
    | [] -> []
    | [ s ] -> [ s ]
    | (x1, x2) :: (x3, x4) :: rest when x1 = x3 ->
        mergeSegments ((x1, max x2 x4) ::rest)
    | (x1, x2) :: (x3, x4) :: rest when x3 <= (x2 + 1) ->
        mergeSegments ((x1, max x2 x4) :: rest)
    | (x1, x2) :: (x3, x4) :: rest when x3 > (x2 + 1) ->
        (x1, x2) :: mergeSegments ((x3, x4) :: rest)

let beaconsOnLine y (sensors:Sensor list) =
    sensors 
    |> List.map (fun x -> x.NearestBeacon) 
    |> List.distinct
    |> List.filter (fun x -> x.y = y)
    |> List.length

let rec limit (maxx: int) (s: (int * int) list) : (int * int) list =
    match s with
    | [] -> []
    | (x1, _) :: _ when x1 > maxx -> []
    | (_, x2) :: rest when x2 < 0 -> limit maxx rest
    | (x1, x2) :: rest ->
        (max x1 0l, min x2 maxx) :: (limit maxx rest)


let segsOnLine (lineNo: int) cut_x (sensors:Sensor list) =
    sensors
    |> List.choose (segmentOnLine lineNo)
    |> List.sort
    |> (fun x -> match cut_x with |Some cc -> limit cc x |> List.sort |None -> x)
    |> mergeSegments


let run filename y cut_x = 
    let sensors = 
        load folder filename |> Array.map Parse.parseLine |> Array.toList
    sensors
    |> segsOnLine y cut_x
    |> List.map (fun (x1,x2) -> x2 - x1 + 1)
    |> List.sum
    |> (fun x -> x - beaconsOnLine y sensors)

let run2 filename cut = 
    let sensors = 
        load folder filename |> Array.map Parse.parseLine |> Array.toList

    let y,segs = 
        [0..cut]
        |> List.map (fun y -> y,(segsOnLine y (Some cut) sensors))
        |> List.filter (fun (y,s) -> s.Length > 1)
        |> List.head 
    let x = segs |> List.head |> snd |> ((+)1)

    let tuning = 4000000L * int64 x + int64 y
    tuning

let test1 = run "test1.txt" 10 None
let part1 = run "input.txt" 2000000 None

let test2 = run2 "test1.txt" 20
let part2 = run2 "input.txt" 4000000

printfn "%i" part1
printfn "%i" part2




