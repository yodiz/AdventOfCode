#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC



type Rule = |Match of char |All of int list |Or of (int list*int list)


let parseRule (str:string) = 
    let id, rule = str |> Text.split2 ":"
    id |> Parse.int32,
    if rule.Contains("\"") then
        rule.Replace("\"", "").Trim().ToCharArray() |> function |[|a|] ->  Rule.Match a
    elif rule.Contains("|") then
        let (a,b) =  rule |> Text.split2 "|"
        let a = a.Trim() |> Text.split " " |> Array.map Parse.int32 |> List.ofArray
        let b = b.Trim() |> Text.split " " |> Array.map Parse.int32 |> List.ofArray
        Rule.Or (a,b)
    else
        let a = rule.Trim() |> Text.split " " |> Array.map Parse.int32 |> List.ofArray
        Rule.All a

let rec checkRule (rules:Map<int,Rule>) ruleIndex index (str:char array)  =
    let ind = String.replicate index "   "
    //printfn "%sExecutring rule %i @ %i" ind ruleIndex index
    let rule = rules |> Map.find ruleIndex
    let check ints = 
        ints
        |> List.fold 
            (fun (ii,s) x -> 
                if s then
                    match checkRule rules x (index+ii) str with
                    |Some f -> 
                        (ii + f, true)
                    |None -> (ii, false)
                else (ii,s)
            ) 
            (0, true)
        |> function |(a,true) -> Some a |_ -> None

    if index >= str.Length  then 
        None
    else 
        match rule with 
        |Rule.Match c -> if str.[index] = c then Some 1 else None
        |Rule.All ints -> ints |> check
        |Rule.Or (a,b) -> 
            

            check a
            |> function |Some s -> Some s |None -> check b
        

let isMatch rules ruleIndex (str:string) = 
    match checkRule rules ruleIndex 0 (str.ToCharArray()) with
    |None -> false
    |Some s when s = str.Length -> true
    |Some s -> false


let loadInp str extrRules = 
    let (rules,msgs) = loadAll str |> Text.split2 "\r\n\r\n"
    let rules = 
        extrRules |> List.fold (fun s (k,v) -> s |> Map.add k v)
            (rules |> Text.split "\r\n" |> Array.map parseRule |> Map.ofArray)
    let m = msgs |> Text.split "\r\n"

    printfn "%A " (rules |> Map.find 8)
    printfn "%A " (rules |> Map.find 11)
    m |> Array.filter (fun x -> isMatch rules 0 x)


let part1 = loadInp "Day19/input.txt" [] |> Array.length


let extraRules = ["8: 42 | 42 8";"11: 42 31 | 42 11 31"] |> List.map parseRule

let test2_1 = loadInp "Day19/test2.txt" [] |> Array.length
let test2_2 = loadInp "Day19/test2.txt" extraRules |> Array.length


//let part2 = loadInp "Day19/input.txt" extraRules |> Array.length
