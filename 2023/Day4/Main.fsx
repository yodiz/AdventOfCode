#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day2
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"

let rec dbl inp = 
    if inp = 0 then 0 
    elif inp = 1 then 1
    else dbl (inp-1) * 2

let parseCard (l:string) = 
    let c, r = Text.split2_noempty ":" l
    let _, c = Text.split2_noempty " " c
    let b, n = Text.split2_noempty "|" r 
    let bricka = b.Trim().Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map Parse.int32 |> Set.ofArray
    let nums   = n.Trim().Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map Parse.int32 |> Set.ofArray
    Parse.int32 c, bricka , nums              

let solve1 input = 
    input
    |> Array.map parseCard
    |> Array.map (fun (c,b,n) -> Set.intersect b n |> Set.count |> dbl)
    |> Array.sum
    
let solve2 input = 
    input
    |> Array.map parseCard
    |> Array.fold 
        (fun s (c,b,n) -> 
            let addM (c:int) (n:int) m = 
                let e = m |> Map.tryFind c |> Option.defaultValue 0
                m |> Map.add c (e + n)

            let matching = Set.intersect b n |> Set.count 
            let s = addM c 1 s
            let copies = s |> Map.tryFind c |> Option.defaultWith (fun () -> failwithf "???")
            let cards = [c+1..c+matching]
            // printfn "Your %i instances of card %i have %i matching numbers, so you win %i copies each of cards [%s]"
            //     copies c matching copies (cards |> List.map (sprintf "%i") |> String.concat ", ")
            cards
            |> List.fold (fun s x -> addM (x) copies s) s

        ) Map.empty
    |> Map.fold (fun s k v -> s+v) 0
    

let aTest = solve1 inptest1
let aTest2 = solve2 inptest1
let part1 = solve1 input 
let part2 = solve2 input
