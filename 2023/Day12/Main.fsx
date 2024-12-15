#if INTERACTIVE
#load "../Common.fsx"
#else 
module Day
#endif

let folder = __SOURCE_DIRECTORY__ + "\\"
let input  = load folder "input.txt"
let inptest1  = load folder "test1.txt"


let allCombos2 jokerChar (options:_ array) (str:string) = 
    let posistions = str.ToCharArray() |> Array.mapi (fun i x -> i,x) |> Array.filter (fun (i,x) -> x = jokerChar) |> Array.map fst
    let len = posistions.Length

    [
        let arr = Array.init posistions.Length (fun x -> 0)
        let arrToStr () = 
            str.ToCharArray() 
            |> Array.mapi 
                (fun i x -> 
                    match posistions |> Array.tryFindIndex (fun c -> c = i) with 
                    |Some p -> options[arr[p]]
                    |None -> str[i]
                )
            |> System.String

        yield arrToStr ()
        let alts = int (float options.Length**len)
        for i = 1 to alts - 1 do
            let mutable x = 0
            while arr[x] = options.Length - 1 do
                arr[x] <- 0
                x <- x + 1            
            arr[x] <- arr[x] + 1
            yield arrToStr ()
    ]

allCombos2 '?' [|'.';'#'|] "???.###"

let isValid str rules = 
    let combos = Text.split_noempty "." str
    combos
    |> Array.map (fun x -> x.Length)
    |> Array.fold 
        (fun (validSoFar, remRules) x -> 
            if validSoFar then 
                match remRules with 
                |[] -> false, remRules
                |r::rem -> 
                    x = r, rem
            else 
                (validSoFar, remRules)
        )
        (true,rules)
    |> (fun (isValidSoFar, remRules) -> isValidSoFar && remRules |> List.isEmpty)
    
isValid "#..##..###..#" [1;1;2;3]

// ???.### 1,1,3 - 1 arrangement
// .??..??...?##. 1,1,3 - 4 arrangements
// ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
// ????.#...#... 4,1,1 - 1 arrangement
// ????.######..#####. 1,6,5 - 4 arrangements
// ?###???????? 3,2,1 - 10 arrangements

let parseLine (str) = 
    let a,b = Text.split2 " " str
    a, b |> Text.split "," |> Array.map Parse.int32 |> List.ofArray

let l,r = parseLine "???.### 1,1,3"

let parse (input) = 
    input |> Array.map parseLine


allCombos2 '?' [| '.';'#' |] "?#?#?#?#?#?#?#?"
|> List.filter (fun x -> isValid x [1;3;1;6]) 


//  ?#?#?#?#?#?#?#?
//  ######.###.#.#.
//  ######.#.###.#.
//  ######.#.#.###.
//  .###.#.#.######
//  .#.###.#.######
//  .#.#.###.######

let solve1 input = 
    parse input
    |> Array.map 
        (fun (x,r) -> 
            let r = 
                allCombos2 '?' [|'.';'#'|] x
                |> List.filter (fun x -> isValid x r)
                |> List.length
            r
        )
    |> Array.sum


// let test1 = solve1 inptest1 //21
// let part1 = solve1 input

// let test2 = solve2 inptest1 // 525152


//Funktion som tar in ett 'uttryck' #???#, och en regellista. 
// Returnerar kvarvarande regler som är kvar efter processat uttryck och hur många varianter som fanns
// Eller ifall reglerna inte går att applicera



let rec processRules (expr:string) rules : Option<int* int list> = 
    let nextDot = expr.IndexOf('.') 
    if nextDot > -1 then
        let nextExpr = expr.Substring(0, nextDot)
        let remaining = expr.Substring(nextDot+1, expr.Length - (nextDot + 1))

        printfn "Dot expr '%s' > '%s' (%s)" nextExpr remaining expr

        let a = 
            if nextExpr = "" then None else processRules nextExpr rules 
            |> function 
                |None -> processRules remaining rules
                |Some (variants, remRules) -> 
                    match processRules remaining remRules with
                    |None -> Some (variants, remRules)
                    |Some (v,r) -> Some (v*variants,r)
        a
    else if expr.Contains("?") then
        let a =
            allCombos2 '?' [| '.';'#' |] expr
            |> List.choose 
                (fun exp ->                     
                    let a = processRules exp rules
                    printfn "Alt - '%s' - %A [%s]" exp a (rules |> List.map (sprintf "%i") |> String.concat ";"); 
                    a
                )

        // printfn "%i alts found - '%s'" a.Length expr
        //Vi behöver bara behålla den som har flest matchade regler? vid lika spelar det ingen roll?
        a
        |> function 
            |[] -> None 
            |things ->
                let r =  
                    things 
                    |> List.minBy (fun (k,x) -> x |> List.length)
                let v = a |> List.filter (fun (k,x) -> x = snd r)
                
                printfn "Tets: %A" v
                // printfn "%i alts found - '%s'" v expr
                r
                |> (fun (k,n) -> v |> List.sumBy (fst), n)
                |> Some
    else
        // printfn "Final Expr '%s' %A" expr rules
        // match rules with expr        
        match rules with     
        |[] -> if expr = "" then Some (1, []) else None
        |a::rest -> 
            if a = expr.Length then
                Some (1, rest)
            else
                None
        


processRules "???" [1]
processRules "????.????" [1;1]
processRules "#." [1;1]
processRules "#.#" [1]
processRules ".#" []

processRules "..." []
processRules "" []

processRules "??.??" [1;1]

processRules "??????????" [1;2]

processRules "##.#" [1]
processRules "...#" [1]

//From tests
processRules "???.###" [1;1;3] // - 1 arrangement
processRules ".??..??...?##." [1;1;3] // - 4 arrangements
processRules "?#?#?#?#?#?#?#?" [1;3;1;6] //- 1 arrangement
processRules "????.#...#..." [4;1;1] //- 1 arrangement
processRules "????.######..#####." [1;6;5] // - 4 arrangements
processRules "?###????????" [3;2;1] // - 10 arrangements


// processRules "#...." [1]


// processRules "#.?#.#???????...??##" [1;2;2;1;1;4]

// processRules "#.??.??.??" [1;1;1;1] // 8                

// processRules ".#" [1]

// processRules "???.###????.###????.###????.###????.###" [1;1;3;1;1;3;1;1;3;1;1;3;1;1;3] // 1


let solve2 input =  
    parse input
    |> Array.map 
        (fun (x,r) -> 
            let x = Array.init 5 (fun _ -> x) |> String.concat "?"
            let rules = [r;r;r;r;r] |> List.collect id
            let r = 
                processRules x rules
                |> Option.get
                |> fst

            printfn "%s - %i - %s" x r (rules |> List.map (sprintf "%i") |> String.concat ";")

            // printfn "%s - %i" x r
            r
        )
    |> Array.sum



//  1,1,3,1,1,3,1,1,3,1,1,3,1,1,3
// ???.###????.###????.###????.###????.### 

// solve2 [|"???.### 1,1,3"|] //1
// solve2 [|"????.#...#... 4,1,1"|] //16
// solve2 [|"????.######..#####. 1,6,5"|] //2500


// solve2 [|"#.?#.#???????...??## 1,2,2,1,1,4"|]

// let part2 = solve2 input 
// let part2test = solve2 inptest1
