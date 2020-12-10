#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC
let inp = load "Day9/input.txt" |> Array.map Parse.int64

let part1 =
    inp |> Array.foldi
        (fun s f x -> 
            if s >= 25 then
                let ns = inp.[s-25..s-1] 
                Array.allPairs ns ns
                |> Array.exists (fun (a,b) -> a + b = x && a <> b)
                |> function |false -> Some x |true -> f
            else 
                f)
        (None)
    |> Option.get
    

let part2 = 
    inp
    |> Array.fold 
        (fun (s,f2) x -> 
            let n,f = 
                s 
                |> List.map (fun l -> x :: l)
                |> List.fold 
                    (fun (s,s2) l -> 
                        match l |> List.sum with
                        |a when a > part1 -> s,s2
                        |a when a = part1 -> s,l::s2
                        |_ -> l::s,s2
                    )
                    ([],[])

            [x] :: n, List.append f f2
        )
        ([], [])
    |> snd
    |> (function [x] -> List.min x + List.max x)


