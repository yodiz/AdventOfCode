#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

let parseRule (s:string) = 
    let (bag, bags) = s |> Text.split2 " contain "
    let bags = bags.TrimEnd('.')
    bag |> Text.split " " |> (function [|a;b;_|] -> a+ " " + b |a -> failwithf "#1 %A" a),
    if bags = "no other bags" then []
    else
        bags |> Text.split ", " |> Array.toList  
        |> List.map (fun x -> x |> Text.split " " |> function |[|Int32 n; a; b;_|] -> n, a + " " + b |a -> failwithf "#2 %A" a)


let rec contain rules wanted bags times = 
    rules
    |> Array.fold 
        (fun s (bag, allowed) -> 
            allowed 
            |> List.fold 
                (fun is (n,b) -> 
                    if b = wanted then
                        let newMap = contain rules bag is n
                        let add = n * times
                        newMap
                        |> Map.tryFind bag
                        |> function 
                            |None -> newMap |> Map.add bag add
                            |Some q -> newMap |> Map.add bag  (add+q)

                    else 
                        is)
                s
        )
        bags 

let test1 = 
    let rules = 
        load "Day7/test1.txt"
        |> Array.map (parseRule)
    contain rules "shiny gold" Map.empty 1
    |> Map.count
    |> Test.equal "Test1" 4
    

let part1 = 
    let rules = 
        load "Day7/input.txt"
        |> Array.map (parseRule)
    contain rules "shiny gold" Map.empty 1
    |> Map.count

let rec two (rules:(string * (int * string) list) []) bag nth = 
    rules
    |> Array.tryFind (fun (b,_) -> b = bag)
    |> function 
        |None -> failwithf "Test"
        |Some (bag,allowed) ->
            let toAdd = 
                allowed
                |> List.fold (fun s (n,bag) -> s + ((two rules bag n) * nth)) 
                    nth
            toAdd
            
    
let part2 = 
    let rules = 
        load "Day7/input.txt"
        |> Array.map (parseRule)
    two rules "shiny gold" 1 - 1
    



