#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day2
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"
let inptest2  = load folder "test2.txt"
    
let parseColors (l:string) =      
    let n, c = Text.split2 " " (l.Trim())
    c.Trim(), n.Trim() |> Parse.int32

let parseGame (l:string) = 
    let g, r = Text.split2 ":" l
    let gameNr = Text.split2 " " g |> snd |> Parse.int32
    let sets = r.Split(';')
    let sets2 = 
        sets 
        |> Array.map (fun x -> x.Split(',') |> Array.map parseColors |> Map.ofArray)
    {| Game = gameNr; 
       MaxSet = 
        r.Split(';')
        |> Array.map (fun x -> x.Split(',') |> Array.map parseColors |> Map.ofArray)
        |> Array.fold 
            (fun s x -> 
                Seq.append (Map.keys s) (Map.keys x) 
                |> Seq.map 
                    (fun k -> 
                        let a = s |> Map.tryFind k |> Option.defaultValue 0 
                        let b = x |> Map.tryFind k |> Option.defaultValue 0 
                        k, max a b
                    )
                |> Map.ofSeq
            )
            Map.empty
    |}

let solve1 input = 
    input
    |> Array.map parseGame
    |> Array.filter 
        (fun (x) -> 
            // 12 red cubes, 13 green cubes, and 14 blue        
            let findColor color = x.MaxSet |> Map.tryFind color |> Option.defaultValue 0
            findColor "red" <= 12 && findColor "green" <= 13 && findColor "blue" <= 14
        )
    |> Array.sumBy _.Game 

let solve2 input = 
    input
    |> Array.map parseGame
    |> Array.map 
        (fun (x) -> 
            let power = x.MaxSet |> Map.toArray |> Array.map snd |> Array.reduce (fun a b -> a*b)
            power
        )
    |> Array.sum

Test.equal "" 8 (solve1 inptest1)
Test.equal "" 2286 (solve2 inptest1)

let part1 = solve1 input
let part2 = solve2 input

//
printfn "Part1: %i" part1
printfn "Part2: %i" part2