#if INTERACTIVE
#load "../Common.fsx"
#else
module Day11
#endif

open System
open AoC

type Monkey = {
    Id : int
    Items : int list
    InspectedItems : int
    Operation : int -> int
    Test : int -> bool
    IfTrue : int
    IfFalse : int
}

let folder = __SOURCE_DIRECTORY__ + "\\"

let parseOp (s:string) = 
    Regexp.matchOrFail "new = (.+) (.) (.+)" s
    |> function [exp1;op;exp2] -> 
                    (fun old -> 
                        let exp1 = if exp1 = "old" then old else Parse.int32 exp1
                        let exp2 = if exp2 = "old" then old else Parse.int32 exp2
                        let op = match op with |"+" -> (+)|"*" -> (*) |a -> failwithf "OP:%s" a
                        op exp1 exp2
                    )
    //new = old * 19

let input  = 
    loadAll folder "input.txt" 
    |> AoC.Text.split "\r\n\r\n"
    |> Array.map 
        (fun x -> 
            let lines = Text.split "\r\n" x

            let monkey = lines[0] |> Text.split2 " " |> snd |> Text.trimc [|':'|] |> Parse.int32
            let items = lines[1] |> Text.split2 ": " |> snd |> Text.split ", " |> Array.map Parse.int32
            let op = lines[2] |> Text.split2 ": " |> snd |> parseOp
            let test = lines[3] |> Text.split2 "divisible by " |> snd |> Parse.int32 |> (fun x -> (fun d -> d % x = 0))
            let ifTrue = lines[4] |> Text.split2 "throw to monkey " |> snd |> Parse.int32
            let ifFlase = lines[5] |> Text.split2 "throw to monkey " |> snd |> Parse.int32
            {
                Id = monkey
                Items = items |> List.ofArray
                InspectedItems = 0
                Operation = op
                Test = test
                IfTrue = ifTrue
                IfFalse = ifFlase
            }
        )
    |> Array.map (fun x -> x.Id, x) 
    |> Map.ofArray


let round (m:Map<int,Monkey>) =
    m
    |> Map.fold 
        (fun m id _monkey -> 
            let monkey = m |> Map.find id
            let kuk = 
                
                monkey.Items
                |> List.map (fun x -> 
                                let w = (monkey.Operation x) / 3
                                let b = monkey.Test w
                                let newM = if b then monkey.IfTrue else monkey.IfFalse
                                if newM = monkey.Id then failwithf "Throwing to self!"
                                w, newM
                            )
                |> List.fold 
                    (fun m (w, newM) ->
                        let x = m |> Map.find newM
                        m |> Map.add newM { x with Items = List.append x.Items [w] }
                    ) m 
               
            //Modyfy current
            kuk |> Map.add monkey.Id { monkey with Items = []; InspectedItems = monkey.InspectedItems + monkey.Items.Length }
        )
        m

let part1 =
    [0..20-1]
    |> List.fold (fun s _x -> round s) input
    |> Map.toList 
    |> List.map snd
    |> List.sortByDescending (fun x -> x.InspectedItems)
    |> List.take 2
    |> function [m1; m2] -> m1.InspectedItems * m2.InspectedItems

    

let part2 = 0


//
printfn "%i" part1
printfn "%i" part2

