#if INTERACTIVE
#load "../Common.fsx"
#else
module DayT
#endif

open System
open AoC

let folder = __SOURCE_DIRECTORY__ + "\\"


type State = {
    Map : byte array array
    Start : int*int
    Goal : int*int
}

let load file = 
    let mutable s = 0,0
    let mutable e = 0,0
    let parseLine y (str:string) = 
        str.ToCharArray() |> Array.mapi (fun x -> function |'S' -> s <- x,y; 'a' |'E' ->  e <- x,y; 'z' |a -> a) |> Array.map byte

    {Map = load folder file |> Array.mapi parseLine; Start = s; Goal = e}


//let canWalk (map:byte array array)  (x,y) (t_x, t_y) = 
//    if t_x < 0 || t_x >= map[0].Length || t_y < 0 || t_y >= map.Length then false
//    else
//        let c = map[y][x]
//        let t = map[t_y][t_x]
//        t <= c + 1uy

let rec walkNext (dict:System.Collections.Generic.Dictionary<_,_>) (map:byte array array) goal (x,y) fromValue visited gCont =

    if visited |> List.contains (x,y) then gCont None
    elif x < 0 || x >= map[0].Length || y < 0 || y >= map.Length then gCont None
    elif (map[y][x]) <= fromValue + 1uy then         
        if (x,y) = goal then
            printfn "Found goal %A" (visited |> List.rev)
            gCont (Some (visited |> List.length))
        else
            match dict.TryGetValue((x,y)) with
            |true,x -> x
            |_ -> 
                printfn "Walking (%i,%i) from %c to %c" x y (char fromValue) (char (map[y][x]))
                let tryWalk (t_x,  t_y) cont = 
                    let newVisisted = (x,y)::visited
                    walkNext dict map goal (t_x, t_y) (map[y][x]) newVisisted (fun x -> cont x)
                

                tryWalk (x,y-1) 
                    (fun up -> tryWalk (x,y+1) 
                                (fun down -> tryWalk (x-1,y) 
                                                (fun left -> tryWalk (x+1,y) 
                                                                (fun right -> 
                                                                    let v = match [up;down;left;right] |> List.choose (fun x -> x) with |[] -> None |x -> x |> List.min |> Some
                                                                    dict.Add((x,y),v)
                                                                    v 
                                                                )
                                                )
                                )
                    )
                    //|> (fun v -> dict.Add((x,y),v); v)
                    |> gCont
    else gCont None


let calc input = (walkNext (System.Collections.Generic.Dictionary<_,_>()) input.Map input.Goal input.Start ('a' |> byte) [] (fun x -> x) |> Option.get)

let test1 = AoC.Test.equal "Test1" 31 (calc (load "test1.txt"))

let part1 = calc (load "input.txt")

let part2 = 0


//
//printfn "%i" part1
//printfn "%i" part2


