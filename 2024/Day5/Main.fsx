#if INTERACTIVE
#load "../Common.fsx"
#else 
module AoC
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = loadAll folder "input.txt"
let test1  = loadAll folder "test1.txt"

let checkRule (update:int array) (before:int,after:int) = 
    let beforeLoc = update |> Array.tryFindIndex ((=)before)
    let afterLoc = update |> Array.tryFindIndex ((=)after)
    
    match beforeLoc, afterLoc with 
    |None, None
    |None, Some _ 
    |Some _, None -> true
    |Some b, Some a -> b < a
    

// 13 47 75
// 
// 47|13 <-- Applicera först!
// 75|47 
// Ta regler först där höger-sidans regler redan uppfyllts
let applyRules (rules:(int*int) array) (update:int array) =
    //Loopa igenom regler, och swappa tills ok?

    let mutable current = update
    
    while (rules |> Array.forall (checkRule current) |> not) do
        rules
        |> Array.iter 
            (fun (before,after) -> 
                if checkRule update (before,after) |> not then
                    let a = update |> Array.findIndex ((=)before)
                    let b = update |> Array.findIndex ((=)after)
                
                    let c = update[a]
                    update[a] <- update[b]
                    update[b] <- c

            )


    printfn "Changed %s to %s" (update |> Array.map (sprintf "%i") |> String.concat ",") (current |> Array.map (sprintf "%i") |> String.concat ",")
    update        

//75,97,47,61,53 becomes 97,75,47,61,53.


let p1 a = 
    let rules, updates = a |> Text.split2 "\r\n\r\n"
    let rules = 
        rules |> Text.split "\r\n" 
        |> Array.map (Text.split2 "|" >> (fun (a,b) -> Parse.int32 a, Parse.int32 b))
    let updates = 
        updates |> Text.split "\r\n" |> Array.map (fun x -> Text.split "," x |> Array.map Parse.int32)
    
    updates 
    |> Array.filter (fun x -> rules |> Array.forall (checkRule x))
    |> Array.map (fun x -> x[(x.Length-1) /2])
    |> Array.sum

let p2 a =
    let rules, updates = a |> Text.split2 "\r\n\r\n"
    let rules = 
        rules |> Text.split "\r\n" 
        |> Array.map (Text.split2 "|" >> (fun (a,b) -> Parse.int32 a, Parse.int32 b))
    let updates = 
        updates |> Text.split "\r\n" |> Array.map (fun x -> Text.split "," x |> Array.map Parse.int32)
    
    updates 
    |> Array.filter (fun x -> rules |> Array.forall (checkRule x) |> not)
    |> Array.map (applyRules rules)
    //|> Array.map (fun x -> printfn "%s" (String.concat "," (x |> Array.map (sprintf "%i"))); x)
    |> Array.map (fun x -> x[(x.Length-1) /2])
    |> Array.sum

Test.equal "Test1" 143 (p1 test1)
Test.equal "Test2" 123 (p2 test1)

let part1 = p1 input
let part2 = p2 input

printfn "Part1: %i" part1
printfn "Part2: %i" part2
