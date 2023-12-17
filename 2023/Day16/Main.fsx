#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"
let inptest2  = load folder "test2.txt"

type Beam = { Pos : Pos; Dir : Pos }

[<TailCallAttribute>]
let rec tickBeams (map:Pos.Map) preEnergized toProcess (beam:Beam) = 
    // printfn "%i,%i" beam.Pos.x beam.Pos.y
    let energized = (preEnergized |> Set.add beam)
    // let c = System.Console.ReadKey() 
    // if c.KeyChar = 'q' then failwithf "QUIT"
    let nextPos = Pos.add beam.Pos beam.Dir
    
    if nextPos.x < 0 || nextPos.y < 0 || nextPos.x > int64 map.Width - 1L || nextPos.y > int64 map.Height - 1L 
        || preEnergized |> Set.contains beam
    then
        
        //Out of bound process next
        match toProcess with 
        |[] -> energized
        |a::rest -> 
            tickBeams map energized rest a
    else
        //    - | / \
        let t = map.Map |> Map.tryFind nextPos
        
        match t with 
        |None -> 
            tickBeams map energized toProcess { beam with Pos = nextPos } 
        |Some c ->
            match c with 
            |'-' -> 
                //Om vi rör hos horizontelt gör inget
                if beam.Dir = Pos.Dir.West || beam.Dir = Pos.Dir.East then
                    tickBeams map energized toProcess { beam with Pos = nextPos } 
                else // , annars splittar vi  åt varisitt håll
                    tickBeams map energized ({ beam with Pos = nextPos; Dir = Pos.Dir.East }::toProcess) { beam with Pos = nextPos; Dir = Pos.Dir.West } 
            |'|' -> 
                //Om vi rör hos vertical gör inget
                if beam.Dir = Pos.Dir.North || beam.Dir = Pos.Dir.South then
                    tickBeams map energized toProcess { beam with Pos = nextPos } 
                else // , annars splittar vi  åt varisitt håll
                    tickBeams map energized ({ beam with Pos = nextPos; Dir = Pos.Dir.North }::toProcess) { beam with Pos = nextPos; Dir = Pos.Dir.South }             
            |'/' -> 
                let m = [Pos.Dir.North, Pos.Dir.East;
                         Pos.Dir.South, Pos.Dir.West;
                         Pos.Dir.East, Pos.Dir.North;
                         Pos.Dir.West, Pos.Dir.South
                         ]
                let dir = m |> List.find (fun (d,t)  -> beam.Dir = d) |> snd
                tickBeams map energized toProcess { beam with Pos = nextPos; Dir = dir } 
            |'\\' -> 
                let m = [Pos.Dir.North, Pos.Dir.West;
                         Pos.Dir.South, Pos.Dir.East;
                         Pos.Dir.West, Pos.Dir.North;
                         Pos.Dir.East, Pos.Dir.South]
                let dir = m |> List.find (fun (d,t)  -> beam.Dir = d) |> snd
                tickBeams map energized toProcess { beam with Pos = nextPos; Dir = dir }             
            |c -> failwithf "WTF? %c" c


let getEnergized map beam = 
    let energized = tickBeams map Set.empty [] beam
    let energized = energized |> Set.map (fun x -> x.Pos) 
    let emap = energized |> Set.fold (fun s x -> s |> Map.add (x) '#') map.Map
    // Pos.Map.print { map with Map = emap }
    (energized |> Set.count) - 1 
            

let solve1 input = 
    let map = Pos.Map.parse '.' input
    let beam = { Pos = Pos.create -1 0; Dir = Pos.Dir.East }
    getEnergized map beam

let solve2 input =  
    //För varje  edge, räkna ut max energized 

    let map = Pos.Map.parse '.' input
    
    let topBottom = 
        Array.init map.Width (fun x -> x)
        |> Array.map 
            (fun x -> 
                [|
                    { Pos = Pos.create x -1; Dir = Pos.Dir.South }
                    { Pos = Pos.create x map.Height; Dir = Pos.Dir.North }
                |]
            )
    let leftRight = 
        Array.init map.Height (fun x -> x)
        |> Array.map 
            (fun y -> 
                [|
                    { Pos = Pos.create -1 y; Dir = Pos.Dir.East }
                    { Pos = Pos.create map.Width y; Dir = Pos.Dir.West }
                |]
            )

    let beams = 
        topBottom 
        |> Array.append leftRight
        |> Array.collect id

    beams 
    |> Array.fold (fun s beam -> max s (getEnergized map beam)) 0


let test1 = solve1 inptest1 
let test2 = solve2 inptest1 // 51
let part1 = solve1 input
let part2 = solve2 input 
