#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day2Part2
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = loadAll folder "input.txt"
let inptest1  = loadAll folder "test1.txt"

[<StructuredFormatDisplay("Range({From} to {To} (Len={Length}))")>]
type Range = { From:int64; Length: int64 } 
    with 
        member x.To = x.From + x.Length - 1L
        member x.ToExcl = x.To + 1L
module Range = 
    let fromLen from len = { From = from; Length = len } 
    let fromTo fromN toN = fromLen fromN (toN - fromN + 1L)

    ///Returns the range that is overlapping or None if not overlapping
    let overlap_r (r1:Range) (r2:Range) = 
        let totalRange = (max r1.ToExcl r2.ToExcl) - (min r1.From r2.From)
        let sumOfRanges = (r1.ToExcl - r1.From) + (r2.ToExcl - r2.From)
        if sumOfRanges > totalRange then
            let start = (max r1.From r2.From) 
            let len = (min r1.ToExcl r2.ToExcl) - (max r1.From r2.From)
            let rhit = { From = start; Length = len }

            let r1From = min r1.From r2.From
            let r1Len = start - r1From
            let rbefore = { From = r1From; Length = r1Len }

            let r2End = max r1.ToExcl r2.ToExcl 
            let r2Start = rhit.ToExcl
            let r2Len = r2End - r2Start
            let rafter = { From = r2Start; Length = r2Len }

            Some (rbefore,rhit,rafter)
        else None

    let subtractIntervals (a: int64, b: int64) (c: int64, d: int64) =
        if b < c || d < a then [(a, b)]
        else if a < c && b > d then [(a, c - 1L); (d + 1L, b)]
        else if a < c then [(a, c - 1L)]
        else [(d + 1L, b)]

    let Subtract (target:Range) (toSubtract:Range) = 
        subtractIntervals (target.From, target.To) (toSubtract.From, toSubtract.To)
        |> List.filter (fun (f,t) -> f <= t)
        |> List.map (fun (f,t) -> { From = f; Length = t-f+1L })
        |> List.toArray


type Mapper = { Dest : int64; Source : Range }
module Mapper = 
    ///For a Range, apply mapper and return 
    /// new ranges and indicate if they have remapped or not with bool
    let mapper2 (r:Range) (mps:Mapper) : (bool*Range) array = 
        match Range.overlap_r r mps.Source with
        |None -> [|false,r|] 
        |Some (before,hit,after) -> 
            let before = Range.Subtract before mps.Source
            let after = Range.Subtract after mps.Source
            let delta = mps.Source.From - mps.Dest

            [| false,before; true,[|{ hit with From = hit.From - delta }|] ; false,after |]
            |> Array.collect (fun (b,r) -> r |> Array.map (fun x -> b,x))
    ///Apply mapper one by one to unmapped-ranges
    /// Return ranges and indicate if they have been mapped or not
    let rec mappers3 (r:(bool*Range) array) (mps:Mapper array) = 
        match mps with 
        |[||] -> r
        |a -> 
            let newRanges = 
                r
                |> Array.collect 
                    (fun (matched,r) -> 
                        if matched then [|true,r|]
                        else
                            mapper2 r a[0]
                    )
            mappers3 newRanges a[1..]
    ///Apply mappers one by one to range and return new ranges that have been mapped
    let mappers2 range mappers = 
        mappers3 [|false,range|] mappers
        |> Array.map snd

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
                    |> Array.map (fun p -> { Dest = p[0]; Source = Range.fromLen p[1] p[2]})

                {| Index = i; Name = name; Mapper = p |}
            )
    {| Seeds = seeds; X = maps |}
   
let solve2 input = 
    let p = parse input    
    p.Seeds
    |> Array.chunkBySize 2
    |> Array.map (function [|start;len|] -> Range.fromLen start len )
    |> Array.map 
        (fun seed -> 
            p.X
            |> Array.fold 
                (fun seeds x -> 
                    seeds |> Array.collect (fun r -> Mapper.mappers2 r x.Mapper)
                ) 
                [|seed|]
        )
    |> Array.collect (fun x -> x)
    |> Array.map (fun x -> x.From)
    |> Array.min

let aTest2 = solve2 inptest1
Test.equal "Test2" 46L (solve2 inptest1)
let part2 = (Time.func solve2) input
