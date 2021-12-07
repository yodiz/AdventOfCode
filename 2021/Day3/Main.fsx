#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day3
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let parseLine (str:string) = 
    str.ToCharArray() |> Array.map (function |'1' -> true |'0' -> false |_ ->  failwithf "")

let input  = 
    load folder "input.txt" |> Array.map parseLine

let part1 = 
    let count = 
        input
        |> Array.fold 
            (fun s x -> s |> Array.mapi (fun i n -> if x.[i] then n + 1 else n))
            (input.[0] |> Array.map (fun _ -> 0))

    let calc fn = 
        count
        |> Array.map (fun x -> fn x)
        |> String.concat ""
        |> (fun x -> System.Convert.ToInt32(x,2))

    let gamma = calc (fun x -> if x > input.Length / 2 then "1" else "0")
    let epsilon = calc (fun x -> if x > input.Length / 2 then "0" else "1")

    gamma * epsilon
        


let part2 = 
    let rec calc index (inp:bool array array) rule =
        if inp.Length = 1 then 
            inp.[0]
            |> Array.map (fun x -> if x then "1" else "0") |> String.concat "" |> (fun x -> System.Convert.ToInt32(x,2))   
        else
            let ones = 
                inp |> Array.fold (fun s x -> if x.[index] then s+1 else s) 0
            let zeros = inp.Length - ones
            let toSave = rule ones zeros 
            let newInp = 
                inp
                |> Array.filter (fun x -> x.[index] = toSave)
            calc (index+1) newInp rule


    let oxygen = calc 0 input (fun ones zeroes -> ones >= zeroes)
    let co2 = calc 0 input (fun ones zeroes -> ones < zeroes)

    oxygen*co2

    


