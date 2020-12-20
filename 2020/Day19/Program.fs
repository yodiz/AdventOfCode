#if INTERACTIVE
#load "../Common.fsx"
#endif
open AoC

type Rule = |Match of char |All of int list |Or of (int list*int list)

type S = {
    ToMatch : char list
    ToSatisfy : int list
    Rules : Map<int,Rule>
}


let rec rule (s:S) : bool = 
    match s.ToSatisfy, s.ToMatch with
    |[], [] -> true
    |a :: rest, b :: restM -> 
        match s.Rules |> Map.find a with 
        |Rule.Match c  -> 
            if c = b then rule { s with ToMatch = restM; ToSatisfy = rest }
            else false
        |Rule.All ints ->
            rule { s with ToSatisfy = List.append ints rest }
        |Rule.Or (a,b) -> 
            rule { s with ToSatisfy = List.append a rest }
            || rule { s with ToSatisfy = List.append b rest }
    |a,b -> false

let create (str:string) = str.ToCharArray() |> Array.toList
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

let loadInp str extrRules = 
    let (rules,msgs) = loadAll str |> Text.split2 "\r\n\r\n"
    let rules = 
        extrRules 
        |> List.fold 
            (fun s (k,v) -> s |> Map.add k v)
            (rules |> Text.split "\r\n" |> Array.map parseRule |> Map.ofArray)
    let m = msgs |> Text.split "\r\n"
    m |> Array.filter (fun x -> rule { ToMatch = create x; ToSatisfy = [0]; Rules = rules })


let test1 = loadInp "Day19/test1.txt" [] |> Array.length |> Test.equal "test1" 2
let part1 = loadInp "Day19/input.txt" [] |> Array.length

let extraRules = ["8: 42 | 42 8";"11: 42 31 | 42 11 31"] |> List.map parseRule
let test2_1 = loadInp "Day19/test2.txt" [] |> Array.length |> Test.equal "test1" 3
let test2_2 = loadInp "Day19/test2.txt" extraRules |> Array.length |> Test.equal "test1" 12
let part2 = loadInp "Day19/input.txt" extraRules |> Array.length
