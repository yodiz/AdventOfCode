#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day2
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = loadAll folder "input.txt"
let inptest1  = loadAll folder "test1.txt"

type Mapper = {Dest : int64; Source : int64; Length : int64}
module Mapper = 
    let mapper (s:int64) (mapper:Mapper) =
        if s >= mapper.Source && s < mapper.Source + mapper.Length then
            Some (s - mapper.Source +  mapper.Dest)
        else None
    let mappers s (mps:Mapper array) = 
        mps |> Array.tryPick (mapper s) |> Option.defaultValue s

    

let parse (input:string) = 
    let t = input.Split("\r\n\r\n", System.StringSplitOptions.RemoveEmptyEntries)
    let _,seeds = Text.split2 ":" t[0]
    let seeds = seeds.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map Parse.int64

    let maps = 
        t[1..]
        |> Array.mapi 
            (fun i c -> 
                let t = c.Split("\r\n")
                let name = t[0]
                let p = 
                    t[1..] |> Array.map (fun x -> x.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map Parse.int64)
                    |> Array.map (fun p -> { Dest = p[0]; Source = p[1]; Length = p[2] })

                {| Index = i; Name = name; Mapper = p |}
            )

    {|
        Seeds = seeds
        X = maps
    |}
    
let solve1 input = 
    let p = parse input    
    p.Seeds
    |> Array.map 
        (fun seed -> 
            p.X
            |> Array.fold (fun s x -> let a = Mapper.mappers s x.Mapper                                        
                                        // printfn "Mapping '%s' %i -> %i" x.Name s a
                                      a) 
                           seed
        )
    |> Array.min
   
let solve2 input = 
    let p = parse input    
    let xx = 
        p.Seeds
        |> Array.chunkBySize 2
        |> Array.map (function [|start;len|] -> [|start..start+len-1L |])
    xx
    

    //Man borde kunna få ranges för seeds som blir samma  (Utna för rangearna)
    //

    // p.Seeds
    // |> Array.map 
    //     (fun seed -> 
    //         p.X
    //         |> Array.fold (fun s x -> let a = Mapper.mappers s x.Mapper                                        
    //                                     // printfn "Mapping '%s' %i -> %i" x.Name s a
    //                                   a) 
    //                        seed
    //     )
    // |> Array.min
   
let aTest = solve1 inptest1
let aTest2 = solve2 inptest1
let part1 = solve1 input 
// let part2 = solve2 input
