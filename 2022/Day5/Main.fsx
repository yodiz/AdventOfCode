#if INTERACTIVE
#load "../Common.fsx"
#else
module Day5
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"

let parseLine (str:string) = 
    ()

let input  = 
    load folder "input.txt" 

let i = input |> Array.findIndex ((=)"")

let board = 
    let mutable d = Map.empty
    input[0..i-2]
    |> Array.iter 
        (fun x -> 
            x.ToCharArray()
            |> Array.mapi (fun i x -> if (i - 1) % 4 = 0 then Some x else None)
            |> Array.choose id
            |> Array.iteri (fun i x -> if x <> ' ' then match d |> Map.tryFind (i+1) with |None -> d <- d |> Map.add (i+1) [x] |Some l -> d <- d |> Map.add (i+1) (x::l))
        )
    d |> Map.map (fun k v -> v |> List.rev)


let printBoard() = 
    board 
    |> Map.toArray 
    |> Array.map snd 
    |> Array.map (fun x -> x |> List.map (string) |> String.concat "") 
    |> Array.map string 
    |> String.concat " "

printfn "%s %A" (printBoard ()) board

//move 2 from 2 to 8
let moves = 
    input[i+1..]
    |> Array.map (fun x -> Regexp.matchOrFail "move (\\d+) from (\\d+) to (\\d)" x)
    |> Array.fold
        (fun (board:Map<_,_>) x -> 
            match x with 
            |[Int32 n;Int32 f;Int32 t] -> 
                printfn "%i from %i to %i" n f t 


                let things, a = 
                    [0..n-1] 
                    |> List.fold
                        (fun (things, s) _ -> 
                            let x::t = s |> Map.find f 
                            x::things, s |> Map.add f t
                        ) 
                        ([], board)
                
                let xx = 
                    a |> Map.add t (List.append  (things) (a |> Map.find t))
                
                printfn "%s %A" (printBoard ()) xx
                xx

        )
        board



//move 2 from 2 to 8
let moves2 = 
    input[i+1..]
    |> Array.map (fun x -> Regexp.matchOrFail "move (\\d+) from (\\d+) to (\\d)" x)
    |> Array.fold
        (fun (board:Map<_,_>) x -> 
            match x with 
            |[Int32 n;Int32 f;Int32 t] -> 
                printfn "%i from %i to %i" n f t 


                let things, a = 
                    [0..n-1] 
                    |> List.fold
                        (fun (things, s) _ -> 
                            let x::t = s |> Map.find f 
                            x::things, s |> Map.add f t
                        ) 
                        ([], board)
                
                let xx = 
                    a |> Map.add t (List.append  (things |> List.rev) (a |> Map.find t))
                
                printfn "%s %A" (printBoard ()) xx
                xx

        )
        board


let part1 = 
    moves
    |> Map.toArray 
    |> Array.map snd 
    |> Array.map (fun x -> let a = x |> List.toArray in a.[0]) 
    |> Array.map string 
    |> String.concat ""
   

let part2 = 
    moves2
    |> Map.toArray 
    |> Array.map snd 
    |> Array.map (fun x -> let a = x |> List.toArray in a.[0]) 
    |> Array.map string 
    |> String.concat ""


//
printfn "%s" part1
printfn "%s" part2

