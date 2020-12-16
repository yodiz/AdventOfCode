#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

let rec speak maxRound spoken n round  =    
    if round > maxRound then n
    else
        let nn = 
            spoken 
            |> Map.tryFind n
            |> function |None -> 0 |Some (a,b) -> a-b
                    
        let ns = spoken |> Map.changeOrDefault nn (fun (a,b) -> round,a) (round,round)

        if round % 1000000 = 0 then
            printfn "Round: %i, N: %i, NN: %i" round n nn
        speak maxRound ns nn (round+1)
            

let strArr (str:string) = Text.split "," str |> Array.map Parse.int32 |> Array.toList
let speakIt ma init =
    let init = strArr init
    let i,m,l = init |> List.fold (fun (i,s,_) x -> (i+1), (s |> Map.add x ((i+1),(i+1))), x) (0,Map.empty,0)
    speak ma m l (i+1)


let test1 = speakIt 2020 "0,3,6"

let part1 = speakIt 2020 "1,0,18,10,19,6"
let part2 = speakIt 30000000 "1,0,18,10,19,6"
    
