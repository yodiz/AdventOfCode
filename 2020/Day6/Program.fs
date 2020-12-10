#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC


let part1 =     
    loadAll "Day6/input.txt" 
    |> Text.split "\r\n\r\n"
    |> (fun x -> 
        x 
        |> Array.map (fun y -> 
                        y 
                        |> Text.split "\r\n"
                        |> Array.map (fun p -> p.ToCharArray() |> Set.ofArray)
                        |> Array.reduce (Set.union) 
                        )
        |> Array.fold (fun s x -> s + (x |> Set.count)) 0
       )


let part2 = 
    loadAll "Day6/input.txt" 
    |> Text.split "\r\n\r\n"
    |> (fun x -> 
        x 
        |> Array.map (fun y -> 
                        y 
                        |> Text.split "\r\n"
                        |> Array.map (fun p -> p.ToCharArray() |> Set.ofArray)
                        |> Array.reduce (Set.intersect)
                        )
        |> Array.fold (fun s x -> s + (x |> Set.count)) 0
       )
